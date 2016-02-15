/* sfdb_freesound_mootcher.cpp **********************************************************************

	Adapted for Ardour by Ben Loftis, March 2008
	Updated to new Freesound API by Colin Fletcher, November 2011

	Mootcher 23-8-2005

	Mootcher Online Access to thefreesoundproject website
	http://freesound.iua.upf.edu/

	GPL 2005 Jorn Lemon
	mail for questions/remarks: mootcher@twistedlemon.nl
	or go to the freesound website forum

	-----------------------------------------------------------------

	Includes:
		curl.h    (version 7.14.0)
	Librarys:
		libcurl.lib

	-----------------------------------------------------------------
	Licence GPL:

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.


*************************************************************************************/
#include "sfdb_freesound_mootcher.h"

#include "pbd/xml++.h"
#include "pbd/error.h"

#include <sys/stat.h>
#include <sys/types.h>
#include <iostream>

#include <glib.h>
#include "pbd/gstdio_compat.h"

#include "i18n.h"

#include "ardour/audio_library.h"
#include "ardour/debug.h"
#include "ardour/filesystem_paths.h"
#include "ardour/rc_configuration.h"
#include "pbd/pthread_utils.h"

#include "ardour_dialog.h"
#include "gui_thread.h"

using namespace PBD;

static const std::string base_url = "http://www.freesound.org/apiv2";

// Ardour 4
static const std::string default_token = "b2cc51878bd4fde055e3e84591eb289715d01503";
static const std::string client_id = "c7eff9328525c51775cb";

static const std::string fields = "id,name,duration,filesize,samplerate,license,download,previews";

//------------------------------------------------------------------------
Mootcher::Mootcher(const std::string &the_token)
	: curl(curl_easy_init())
{
	DEBUG_TRACE(PBD::DEBUG::Freesound, "Created new Mootcher, oauth_token =\"" + the_token + "\"\n");
	custom_headers = NULL;
	if  (the_token != "") {
		oauth_token = the_token;
		std::string auth_header = "Authorization: Bearer " + oauth_token;
		DEBUG_TRACE(PBD::DEBUG::Freesound, "auth_header = " + auth_header + "\n");
		custom_headers = curl_slist_append (custom_headers, auth_header.c_str());
		curl_easy_setopt(curl, CURLOPT_HTTPHEADER, custom_headers);
	} else {
		oauth_token = "";
	}
	cancel_download_btn.set_label (_("Cancel"));
	progress_hbox.pack_start (progress_bar, true, true);
	progress_hbox.pack_end (cancel_download_btn, false, false);
	progress_bar.show();
	cancel_download_btn.show();
	cancel_download_btn.signal_clicked().connect(sigc::mem_fun (*this, &Mootcher::cancelDownload));
};
//------------------------------------------------------------------------
Mootcher:: ~Mootcher()
{
	curl_easy_cleanup(curl);
	if (custom_headers) {
		curl_slist_free_all (custom_headers);
	}
	DEBUG_TRACE(PBD::DEBUG::Freesound, "Destroyed Mootcher\n");
}

//------------------------------------------------------------------------

void Mootcher::ensureWorkingDir ()
{
	std::string p = ARDOUR::Config->get_freesound_download_dir();

	DEBUG_TRACE(PBD::DEBUG::Freesound, "ensureWorkingDir() - " + p + "\n");
	if (!Glib::file_test (p, Glib::FILE_TEST_IS_DIR)) {
		if (g_mkdir_with_parents (p.c_str(), 0775) != 0) {
			PBD::error << "Unable to create Mootcher working dir" << endmsg;
		}
	}
	basePath = p;
#ifdef PLATFORM_WINDOWS
	std::string replace = "/";
	size_t pos = basePath.find("\\");
	while( pos != std::string::npos ){
		basePath.replace(pos, 1, replace);
		pos = basePath.find("\\");
	}
#endif
}


//------------------------------------------------------------------------
size_t Mootcher::WriteMemoryCallback(void *ptr, size_t size, size_t nmemb, void *data)
{
	int realsize = (int)(size * nmemb);
	struct MemoryStruct *mem = (struct MemoryStruct *)data;

	mem->memory = (char *)realloc(mem->memory, mem->size + realsize + 1);

	if (mem->memory) {
		memcpy(&(mem->memory[mem->size]), ptr, realsize);
		mem->size += realsize;
		mem->memory[mem->size] = 0;
	}
	return realsize;
}


//------------------------------------------------------------------------

std::string Mootcher::sortMethodString(enum sortMethod sort)
{
// given a sort type, returns the string value to be passed to the API to
// sort the results in the requested way.

	switch (sort) {
		case sort_duration_descending:  return "duration_desc";
		case sort_duration_ascending:   return "duration_asc";
		case sort_created_descending:   return "created_desc";
		case sort_created_ascending:    return "created_asc";
		case sort_downloads_descending: return "downloads_desc";
		case sort_downloads_ascending:  return "downloads_asc";
		case sort_rating_descending:    return "rating_desc";
		case sort_rating_ascending:     return "rating_asc";
		default:                        return "";
	}
}

//------------------------------------------------------------------------
void Mootcher::setcUrlOptions()
{
	// basic init for curl
	curl_global_init(CURL_GLOBAL_ALL);
	// some servers don't like requests that are made without a user-agent field, so we provide one
	curl_easy_setopt(curl, CURLOPT_USERAGENT, "libcurl-agent/1.0");
	// setup curl error buffer
	curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, errorBuffer);
	// Allow redirection
	curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1);

	// Allow connections to time out (without using signals)
	curl_easy_setopt(curl, CURLOPT_NOSIGNAL, 1);
	curl_easy_setopt(curl, CURLOPT_CONNECTTIMEOUT, 30);


}

std::string Mootcher::doRequest(std::string uri, std::string params)
{
	std::string result;
	struct MemoryStruct xml_page;
	xml_page.memory = NULL;
	xml_page.size = 0;

	setcUrlOptions();
	curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);
	curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *) &xml_page);

	// curl_easy_setopt(curl, CURLOPT_HTTPGET, 1);
	// curl_easy_setopt(curl, CURLOPT_POSTFIELDS, postMessage.c_str());
	// curl_easy_setopt(curl, CURLOPT_POSTFIELDSIZE, -1);

	// the url to get
	std::string url = base_url + uri + "?";
	if (params != "") {
		url += params + "&token=" + default_token + "&format=xml";
	} else {
		url += "token=" + default_token + "&format=xml";
	}

	curl_easy_setopt(curl, CURLOPT_URL, url.c_str() );

	DEBUG_TRACE(PBD::DEBUG::Freesound, "doRequest() " + url + "\n");

	// perform online request
	CURLcode res = curl_easy_perform(curl);
	if( res != 0 ) {
		std::string errmsg = string_compose (_("curl error %1 (%2)"), res, curl_easy_strerror(res));
		error << errmsg << endmsg;
		DEBUG_TRACE(PBD::DEBUG::Freesound, errmsg + "\n"); 
		return "";
	}

	// free the memory
	if (xml_page.memory) {
		result = xml_page.memory;
	}

	free (xml_page.memory);
	xml_page.memory = NULL;
	xml_page.size = 0;

	DEBUG_TRACE(PBD::DEBUG::Freesound, result + "\n");
	return result;
}


std::string Mootcher::searchSimilar(std::string id)
{
	std::string params = "";

	params += "&fields=" + fields;
	params += "&num_results=100";
	// XXX should we filter out MP3s here, too?
	// XXX and what if there are more than 100 similar sounds?

	return doRequest("/sounds/" + id + "/similar/", params);
}

//------------------------------------------------------------------------

bool
Mootcher::oauth(const std::string &username, const std::string &password)
{
	/* Logging into Freesound requires us to jump through a few hoops.
	 * See http://www.freesound.org/docs/api/authentication.html#token-authentication for the documentation.
	 *
	 * First, we must retrieve the page at:
	 *     https://www.freesound.org/apiv2/oauth2/authorize/?client_id=c7eff9328525c51775cb&response_type=code
	 *
	 * Fortunately, this page, although HTML, is valid XML, so we can use XMLTree and friends to parse out
	 * the hidden form field valus that we'll need at the next step.
	 *
	 * Then, we must POST this page with the users username and password, also passing along the
	 * values of 'csrfmiddlewaretoken' and 'next' that were passed to us in hidden form fields. 'next' is
	 * the address of the next page that will be returned, which will be that of a page with an 'authorize' button on.
	 *
	 * We must next POST this page (with the same csrfmiddlewaretoken value), and the name and value of the 'authorize' button.
	 * Ideally, we'd parse out the <input ...> field of the 'Authorize!' button and post the value therein; unfortunately
	 * the page isn't valid XML, so we can't use XMLTree on it. For now, I've assumed that the button
	 * is always called 'authorize' with a value of 'Authorize!'
	 *
	 * The returned page then contains an authorization code, which we have to exchange for a token, by POSTing to
	 * another page. Again, this isn't valid XML: for this one I've hacked up a parser that just looks for a <div>
	 * containing a 40digit hex number.
	 *
	 * Finally, the returned page this time contains the token value which we should use for subsequent download requests, but
	 * (ha ha) this page is actually JSON, because the POST to the previous page (unlike other JSON pages on freesound.org)
	 * ignores the "&format=xml" parameter.
	 *
	 * The hard-won token then needs to be presented back to freesound.org as part of an 'Authorization: Bearer " HTML
	 * header on all subsequent requests that require OAuth.
	 *
	 * This function, as you might expect from the foregoing description, is rather fragile in the face of
	 * any changes in the HTML that's served by freesound.org. Unfortunately, I can't see any better way of doing this
	 * without them extending their API to make it less convoluted for non-web apps to log in. Other possibilities 
	 * include:
	 *     - use HTMLTree and friends from libxml2 to parse the HTML pages
	 *     - just pop up a link in Ardour when we require the user to log into Freesound,
	 *       and provide a place for them to manually paste the access token.
	 *     - embed a mini-browser into Ardour, and use that to show the Freesound login and token pages.
	 *     - persuade Freesound to change their API.
	 *     
	 */

	CURLcode res;
	XMLTree doc;
	struct MemoryStruct xml_page;
	xml_page.memory = NULL;
	xml_page.size = 0;

	DEBUG_TRACE(PBD::DEBUG::Freesound, "oauth(" + username + ",*****)\n");
	setcUrlOptions();
	curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);
	curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *) &xml_page);

	// http://www.freesound.org/docs/api/authentication.html#token-authentication
	// https://www.freesound.org/apiv2/oauth2/authorize/?client_id=c7eff9328525c51775cb&response_type=code&state="hello!"
	// https://www.freesound.org/apiv2/oauth2/logout_and_authorize/?client_id=c7eff9328525c51775cb&response_type=code&state="hello!"

	std::string oauth_url = "https://www.freesound.org/apiv2/oauth2/logout_and_authorize/?client_id="+client_id+"&response_type=code&state=hello";
	std::string cookie_file = Glib::build_filename (ARDOUR::user_config_directory(), "freesound-cookies");
	std::string oauth_page_str;

	curl_easy_setopt(curl, CURLOPT_URL, oauth_url.c_str());
	curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1);
	curl_easy_setopt(curl, CURLOPT_COOKIEJAR, cookie_file.c_str());
	if (DEBUG_ENABLED(PBD::DEBUG::Freesound)) {
		curl_easy_setopt(curl, CURLOPT_VERBOSE, 1);
	}
	curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_ANYSAFE);

	res = curl_easy_perform(curl);
	if (res != CURLE_OK) {
		DEBUG_TRACE(PBD::DEBUG::Freesound, string_compose("curl failed: %1, error=%2\n", oauth_url, res));
		return false;
	}
	if (!xml_page.memory) {
		DEBUG_TRACE(PBD::DEBUG::Freesound, "curl returned nothing!\n");
		return false;
	}

	oauth_page_str = xml_page.memory;
	free (xml_page.memory);
	xml_page.memory = NULL;
	xml_page.size = 0;

	DEBUG_TRACE(PBD::DEBUG::Freesound, oauth_page_str);
	doc.read_buffer(oauth_page_str.c_str());
	XMLNode *oauth_page = doc.root();
	if (!oauth_page) {
		DEBUG_TRACE(PBD::DEBUG::Freesound, "oauth_page page has no doc.root!\n");
		return false;
	}
	if (strcasecmp (doc.root()->name().c_str(), "html")) {
		DEBUG_TRACE(PBD::DEBUG::Freesound, "root is not <html>\n");
		return false;
	}

	// find all input fields with both name and value properties (there should be at least two of them, for 
	// csrfmiddlewaretoken and next page).
	boost::shared_ptr<XMLSharedNodeList> inputs = doc.find("//input[@name and @value]", oauth_page);
	DEBUG_TRACE(PBD::DEBUG::Freesound, string_compose("found %1 input nodes\n", inputs->size()));

	std::string csrf_mwt = "", next_page = "";
	for (XMLSharedNodeList::const_iterator i = inputs->begin(); i != inputs->end(); ++i) {
		XMLProperty *prop_name  = (*i)->property("name");
		XMLProperty *prop_value = (*i)->property("value");
		if (prop_name && prop_value) {
			std::string input_name  = prop_name->value();
			std::string input_value = prop_value->value();
				DEBUG_TRACE(PBD::DEBUG::Freesound, string_compose("found input name %1, value = %2\n", input_name, input_value));
			if (input_name == "csrfmiddlewaretoken") {
				csrf_mwt  = input_value;
			} else if (input_name == "next") {
				next_page = input_value;
			}
		}
	}

	if (csrf_mwt =="") {
		DEBUG_TRACE(PBD::DEBUG::Freesound, "csrfmiddlewaretoken not found\n");
		return false;
	}

	if (next_page =="") {
		DEBUG_TRACE(PBD::DEBUG::Freesound, "next page not found\n");
		return false;
	}


	DEBUG_TRACE(PBD::DEBUG::Freesound, "\n\n*** about to log in...***\n\n");

	char *next_escaped = curl_easy_escape(curl, next_page.c_str(), 0);
	DEBUG_TRACE(PBD::DEBUG::Freesound, string_compose("next_escaped: %1\n\n", next_escaped));

	curl_easy_setopt(curl, CURLOPT_POST, 4);
	curl_easy_setopt(curl, CURLOPT_COPYPOSTFIELDS, ("csrfmiddlewaretoken=" + csrf_mwt + "&username=" + username + "&password=" + password + "&next=" + next_escaped).c_str());
	free (next_escaped);
	curl_easy_setopt(curl, CURLOPT_REFERER, oauth_url.c_str());

	/* POST the login form */
	DEBUG_TRACE(PBD::DEBUG::Freesound, "*** posting... ***\n\n");
	res = curl_easy_perform (curl);
	if (res != CURLE_OK) {
		DEBUG_TRACE(PBD::DEBUG::Freesound, string_compose("curl failed: %1, error=%2\n", oauth_url, res));
		return false;
	}

	if (!xml_page.memory) {
		DEBUG_TRACE(PBD::DEBUG::Freesound, "curl returned nothing!\n");
		return false;
	}

	oauth_page_str = xml_page.memory;
	free (xml_page.memory);
	xml_page.memory = NULL;
	xml_page.size = 0;

	DEBUG_TRACE(PBD::DEBUG::Freesound, oauth_page_str);
#if FREESOUND_EVER_SENDS_VALID_XML
	if (!doc.read_buffer (oauth_page_str.c_str())) {
		DEBUG_TRACE(PBD::DEBUG::Freesound, "doc.read_buffer() returns false\n");
		return false;
	}
	XMLNode *authorize_page = doc.root();
	if (!authorize_page) {
		DEBUG_TRACE(PBD::DEBUG::Freesound, "authorize page has no doc.root!\n");
		return false;
	}

	authorize_page->dump(std::cerr, "authorize page:");

	// find input fields with name, value, & type properties, i.e. the 'Authorize' button
	boost::shared_ptr<XMLSharedNodeList> buttons = doc.find("//input[@name and @value and @type]", oauth_page);

	std::cerr << "found " << buttons->size() << " buttons\n";

	bool found_auth_button = false;
	for (XMLSharedNodeList::const_iterator i = buttons->begin(); i != buttons->end(); ++i) {
		XMLProperty *prop_name  = (*i)->property("name");
		XMLProperty *prop_value = (*i)->property("value");
		XMLProperty *prop_type  = (*i)->property("type");
		if (prop_name && prop_value) {
			std::string input_name  = prop_name->value();
			std::string input_value = prop_value->value();
			std::string input_type  = prop_type->value();
			std::cerr << "found input name :" << input_name << ", value = " << input_value << std::endl;
			if (input_name == "authorize" && input_type == "submit") {
				char *input_value_escaped = curl_easy_escape(curl, input_value.c_str(), 0);
				curl_easy_setopt(curl, CURLOPT_COPYPOSTFIELDS, ("csrfmiddlewaretoken=" + csrf_mwt + "&" + input_name + "=" + input_value_escaped).c_str());
				free (input_value_escaped);
				found_auth_button = true;
				break;
			}
		}
	}
	if (!found_auth_button) {
		DEBUG_TRACE(PBD::DEBUG::Freesound, "'authorize' button not found\n");
		return false;
	}

#else
	// hard-code the name & value of the "Authorize!" button
	curl_easy_setopt(curl, CURLOPT_COPYPOSTFIELDS, ("csrfmiddlewaretoken=" + csrf_mwt + "&authorize=Authorize%21").c_str());

#endif

	curl_easy_setopt(curl, CURLOPT_POST, 2);

	/* POST the "Authorize!" form */
	res = curl_easy_perform (curl);
	if (res != CURLE_OK) {
		DEBUG_TRACE(PBD::DEBUG::Freesound, string_compose("curl failed: %1, error=%2\n", oauth_url, res));
		return false;
	}

	if (!xml_page.memory) {
		DEBUG_TRACE(PBD::DEBUG::Freesound, "curl returned nothing!\n");
		return false;
	}

	oauth_page_str = xml_page.memory;
	free (xml_page.memory);
	xml_page.memory = NULL;
	xml_page.size = 0;

	DEBUG_TRACE(PBD::DEBUG::Freesound, oauth_page_str);

#if FREESOUND_EVER_SENDS_VALID_XML
	
/* 
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
        "http://www.w3.org/TR/html4/loose.dtd">
<html>
<meta name='viewport' content='width=device-width, initial-scale=0.65' />

    <head>
        <title>Freesound - app authorized</title>
        <style>
            body {
                font-family: Verdana, sans-serif;
                font-size: 11px;
                margin:0px 0px;
                padding:0px;
                text-align:center;
                -webkit-text-size-adjust: 100%;
            }
            .container_main {
                margin: auto;
                padding:20px;
                width:440px;
            }
            .container {
                width:400px;
                background-color: #f3f3f3;
                border-radius:10px;
                padding:20px;
                border: solid 1px #e0e0e0;
            }
        </style>
    </head>
    <body>

        <div class="container_main">
            <div class="container">

                 <img src="/media/images/logo.png"/>
     
                <p>
                    Permission granted to application <strong>Ardour 4</strong>!.
                    <br>Your authorization code:
                
                </p>
                <div style="font-size:14px;font-family:'Courier';">228064a0a575a1bcd69f045cdf4c79c2ec578d6f</div>
            </div>
        </div>

    </body>
</html>
*/

	if (!doc.read_buffer (oauth_page_str.c_str())) {
		DEBUG_TRACE(PBD::DEBUG::Freesound, "doc.read_buffer() of token page returns false\n");
		return false;
	}
	XMLNode *auth_granted_page = doc.root();
	if (!auth_granted_page) {
		DEBUG_TRACE(PBD::DEBUG::Freesound, "auth_granted_page has no doc.root!\n");
		return false;
	}

	auth_granted_page->dump(std::cerr, "auth granted page:");

	// find input fields with name, value, & type properties
	boost::shared_ptr<XMLSharedNodeList> codez = doc.find("//div[@style]", oauth_page);

	std::cerr << "found " << codez->size() << " codez\n";

	for (XMLSharedNodeList::const_iterator i = codez->begin(); i != codez->end(); ++i) {
		XMLProperty *prop_style = (*i)->property("style");
		const std::string content = (*i)->content();

		if (prop_style && content.length() == 40) {
			size_t p = content.find_first_not_of("0123456789abcdefABCDEF");
			if (p == std::string::npos) {
				oauth_token = content;
				return true;
			}
		}
	}
#else
	// hackily parse through the HTML looking for a <div> tag with 40-character hex contents
	size_t p = 0;
	std::string auth_code = "";

	while (true) {
		DEBUG_TRACE(PBD::DEBUG::Freesound, string_compose("searching for \"<div \" from %1\n", p));
		p = oauth_page_str.find("<div ", p);
		if (p == std::string::npos) {
			break;
		}
		DEBUG_TRACE(PBD::DEBUG::Freesound, string_compose("searching for \">\" from %1\n", p));
		size_t q = oauth_page_str.find(">", p);
		if (q == std::string::npos) {
			break;
		}
		DEBUG_TRACE(PBD::DEBUG::Freesound, string_compose("searching for \"</div>\" from %1\n", q));
		size_t r = oauth_page_str.find("</div>", q);
		if (r == std::string::npos) {
			break;
		}
		p = q + 1;
		DEBUG_TRACE(PBD::DEBUG::Freesound, string_compose("checking content length: %1 - %2 = %3\n", r, q, r - q));
		if (r - q != 41) {
			continue;
		}
		std::string content = oauth_page_str.substr(q + 1, 40);
		DEBUG_TRACE(PBD::DEBUG::Freesound, string_compose("checking content is hex: %1\n", content));
		if (content.find_first_not_of("0123456789abcdefABCDEF") != std::string::npos) {
			continue;
		}
		DEBUG_TRACE(PBD::DEBUG::Freesound, "Got authorization code!\n");
		auth_code = content;
		break;
	}
#endif

	if (auth_code == "") { 
		DEBUG_TRACE(PBD::DEBUG::Freesound, "Failed to get authorization code!\n");
		return false;
	}

	curl_easy_setopt(curl, CURLOPT_URL, "https://www.freesound.org/apiv2/oauth2/access_token/");
	curl_easy_setopt(curl, CURLOPT_POST, 5);
	curl_easy_setopt(curl, CURLOPT_COPYPOSTFIELDS, 
			("client_id=" + client_id + 
			"&client_secret=" + default_token + 
			"&grant_type=authorization_code" +
			"&code=" + auth_code).c_str());

	res = curl_easy_perform (curl);
	if (res != CURLE_OK) {
		DEBUG_TRACE(PBD::DEBUG::Freesound, string_compose("curl failed: %1, error=%2\n", oauth_url, res));
		return false;
	}

	if (!xml_page.memory) {
		DEBUG_TRACE(PBD::DEBUG::Freesound, "curl returned nothing!\n");
		return false;
	}

	oauth_page_str = xml_page.memory;
	free (xml_page.memory);
	xml_page.memory = NULL;
	xml_page.size = 0;

	DEBUG_TRACE(PBD::DEBUG::Freesound, oauth_page_str);

	// one of these days ardour's gonna need a proper JSON parser, but not now...
	p = oauth_page_str.find ("access_token");
	oauth_token = oauth_page_str.substr (p + 16, 40);

	DEBUG_TRACE(PBD::DEBUG::Freesound, "oauth_token is :" + oauth_token + "\n");
	return true;

}


std::string Mootcher::searchText(std::string query, int page, std::string filter, enum sortMethod sort)
{
	std::string params = "";
	char buf[24];

	if (page > 1) {
		snprintf(buf, 23, "page=%d&", page);
		params += buf;
	}

	char *eq = curl_easy_escape(curl, query.c_str(), query.length());
	params += "query=\"" + std::string(eq) + "\"";
	free(eq);

	if (filter != "") {
		char *ef = curl_easy_escape(curl, filter.c_str(), filter.length());
		params += "&filter=" + std::string(ef);
		free(ef);
	}

	if (sort)
		params += "&sort=" + sortMethodString(sort);

	params += "&fields=" + fields;
	params += "&page_size=100";

	return doRequest("/search/text/", params);
}

//------------------------------------------------------------------------

std::string Mootcher::getSoundResourceFile(std::string ID)
{

	std::string originalSoundURI;
	std::string audioFileName;
	std::string xml;

	DEBUG_TRACE(PBD::DEBUG::Freesound, "getSoundResourceFile(" + ID + ")\n");

	// download the xmlfile into xml_page
	xml = doRequest("/sounds/" + ID + "/", "");

	XMLTree doc;
	doc.read_buffer( xml.c_str() );
	XMLNode *freesound = doc.root();

	// if the page is not a valid xml document with a 'freesound' root
	if (freesound == NULL) {
		error << _("getSoundResourceFile: There is no valid root in the xml file") << endmsg;
		return "";
	}

	if (strcmp(doc.root()->name().c_str(), "root") != 0) {
		error << string_compose (_("getSoundResourceFile: root = %1, != \"root\""), doc.root()->name()) << endmsg;
		return "";
	}

	XMLNode *name = freesound->child("name");

	// get the file name and size from xml file
	if (name) {

		audioFileName = Glib::build_filename (basePath, ID + "-" + name->child("text")->content());

		//store all the tags in the database
		XMLNode *tags = freesound->child("tags");
		if (tags) {
			XMLNodeList children = tags->children();
			XMLNodeConstIterator niter;
			std::vector<std::string> strings;
			for (niter = children.begin(); niter != children.end(); ++niter) {
				XMLNode *node = *niter;
				if( strcmp( node->name().c_str(), "list-item") == 0 ) {
					XMLNode *text = node->child("text");
					if (text) {
						// std::cerr << "tag: " << text->content() << std::endl;
						strings.push_back(text->content());
					}
				}
			}
			ARDOUR::Library->set_tags (std::string("//")+audioFileName, strings);
			ARDOUR::Library->save_changes ();
		}
	}

	return audioFileName;
}

int audioFileWrite(void *buffer, size_t size, size_t nmemb, void *file)
{
	return (int)fwrite(buffer, size, nmemb, (FILE*) file);
};

//------------------------------------------------------------------------

void *
Mootcher::threadFunc() {
CURLcode res;

	DEBUG_TRACE(PBD::DEBUG::Freesound, "threadFunc\n");
	res = curl_easy_perform (curl);
	fclose (theFile);
	curl_easy_setopt (curl, CURLOPT_NOPROGRESS, 1); // turn off the progress bar

	if (res != CURLE_OK) {
		/* it's not an error if the user pressed the stop button */
		if (res != CURLE_ABORTED_BY_CALLBACK) {
			error <<  string_compose (_("curl error %1 (%2)"), res, curl_easy_strerror(res)) << endmsg;
		}
		remove ( (audioFileName+".part").c_str() );
	} else {
		rename ( (audioFileName+".part").c_str(), audioFileName.c_str() );
		// now download the tags &c.
		getSoundResourceFile(ID);
	}

	return (void *) res;
}

void
Mootcher::doneWithMootcher()
{

	// update the sound info pane if the selection in the list box is still us
	sfb->refresh_display(ID, audioFileName);

	delete this; // this should be OK to do as long as Progress and Finished signals are always received in the order in which they are emitted
}

static void *
freesound_download_thread_func(void *arg)
{
	Mootcher *thisMootcher = (Mootcher *) arg;
	void *res;

	// std::cerr << "freesound_download_thread_func(" << arg << ")" << std::endl;
	res = thisMootcher->threadFunc();

	thisMootcher->Finished(); /* EMIT SIGNAL */
	return res;
}


//------------------------------------------------------------------------

bool Mootcher::checkAudioFile(std::string originalFileName, std::string theID)
{
	DEBUG_TRACE(PBD::DEBUG::Freesound, string_compose("checkAudiofile(%1, %2)\n", originalFileName, theID));
	ensureWorkingDir();
	ID = theID;
	audioFileName = Glib::build_filename (basePath, ID + "-" + originalFileName);

	// check to see if audio file already exists
	FILE *testFile = g_fopen(audioFileName.c_str(), "r");
	if (testFile) {
		fseek (testFile , 0 , SEEK_END);
		if (ftell (testFile) > 256) {
			fclose (testFile);
			DEBUG_TRACE(PBD::DEBUG::Freesound, "checkAudiofile() - found " + audioFileName + "\n");
			return true;
		}

		// else file was small, probably an error, delete it
		DEBUG_TRACE(PBD::DEBUG::Freesound, "checkAudiofile() - " + audioFileName + " <= 256 bytes, removing it\n");
		fclose (testFile);
		remove (audioFileName.c_str() );
	}
	DEBUG_TRACE(PBD::DEBUG::Freesound, "checkAudiofile() - not found " + audioFileName + "\n");
	return false;
}

class CredentialsDialog : public ArdourDialog
{

	public:
		CredentialsDialog(const std::string &title);
		const std::string username() { return username_entry.get_text(); }
		const std::string password() { return password_entry.get_text(); }
	private:
		Gtk::Label username_label;
		Gtk::Entry username_entry;
		Gtk::Label password_label;
		Gtk::Entry password_entry;
};

CredentialsDialog::CredentialsDialog(const std::string &title)
	: ArdourDialog (title, true)
	, username_label (_("User name:"))
	, password_label (_("Password:"))
{
	password_entry.set_visibility (false);
	get_vbox ()->pack_start (username_label);
	get_vbox ()->pack_start (username_entry);
	get_vbox ()->pack_start (password_label);
	get_vbox ()->pack_start (password_entry);

	username_entry.set_activates_default (true);
	password_entry.set_activates_default (true);

	add_button (Gtk::Stock::CANCEL, Gtk::RESPONSE_CANCEL);
	add_button (Gtk::Stock::OK,     Gtk::RESPONSE_ACCEPT);
	set_default_response (Gtk::RESPONSE_ACCEPT);

	show_all ();

}

const std::string
Mootcher::fetchAudioFile(std::string originalFileName, std::string theID, std::string audioURL, SoundFileBrowser *caller)
{

	DEBUG_TRACE(PBD::DEBUG::Freesound, string_compose("fetchAudiofile(%1, %2, %3, ...)\n", originalFileName, theID, audioURL));

	ensureWorkingDir();
	ID = theID;
	audioFileName = Glib::build_filename (basePath, ID + "-" + originalFileName);

	if (!curl) {
		return "";
	}

	if (oauth_token == "") {
		CredentialsDialog freesound_credentials(_("Enter Freesound user name & password"));
		int r = freesound_credentials.run();
		freesound_credentials.hide();
		if (r != Gtk::RESPONSE_ACCEPT) {
			return "";
		}
		while (gtk_events_pending()) {
			// allow the dialogue to become hidden
			gtk_main_iteration ();
		}

		if (!oauth(freesound_credentials.username(), freesound_credentials.password())) {
			return "";
		}

		// oauth() has set a bunch of curl options - reset the important ones now
		curl_easy_setopt(curl, CURLOPT_POST, 0);

		// we don't need to set the 'Authorization:' header here because the instance of
		// curl in this moothcher is still logged in. subsequently created mootchers with
		// the token passed into their constructors will have the header set there.
	}

	// now download the actual file
	theFile = g_fopen( (audioFileName + ".part").c_str(), "wb" );

	if (!theFile) {
		DEBUG_TRACE(PBD::DEBUG::Freesound, "Can't open file for writing:" + audioFileName + ".part\n");
		return "";
	}

	// create the download url
	audioURL += "?token=" + default_token;

	setcUrlOptions();
	curl_easy_setopt(curl, CURLOPT_URL, audioURL.c_str() );
	curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, audioFileWrite);
	curl_easy_setopt(curl, CURLOPT_WRITEDATA, theFile);

	std::string prog;
	prog = string_compose (_("%1"), originalFileName);
	progress_bar.set_text(prog);

	Gtk::VBox *freesound_vbox = dynamic_cast<Gtk::VBox *> (caller->notebook.get_nth_page(2));
	freesound_vbox->pack_start(progress_hbox, Gtk::PACK_SHRINK);
	progress_hbox.show();
	cancel_download = false;
	sfb = caller;

	curl_easy_setopt (curl, CURLOPT_NOPROGRESS, 0); // turn on the progress bar
	curl_easy_setopt (curl, CURLOPT_PROGRESSFUNCTION, progress_callback);
	curl_easy_setopt (curl, CURLOPT_PROGRESSDATA, this);

	Progress.connect(*this, invalidator (*this), boost::bind(&Mootcher::updateProgress, this, _1, _2), gui_context());
	Finished.connect(*this, invalidator (*this), boost::bind(&Mootcher::doneWithMootcher, this), gui_context());
	pthread_t freesound_download_thread;
	pthread_create_and_store("freesound_import", &freesound_download_thread, freesound_download_thread_func, this);

	return oauth_token;
}

//---------

void
Mootcher::updateProgress(double dlnow, double dltotal)
{
	if (dltotal > 0) {
		double fraction = dlnow / dltotal;
		// std::cerr << "progress idle: " << progress_bar.get_text() << ". " << dlnow << " / " << dltotal << " = " << fraction << std::endl;
		if (fraction > 1.0) {
			fraction = 1.0;
		} else if (fraction < 0.0) {
			fraction = 0.0;
		}
		progress_bar.set_fraction(fraction);
	}
}

int
Mootcher::progress_callback(void *caller, double dltotal, double dlnow, double /*ultotal*/, double /*ulnow*/)
{
	// It may seem curious to pass a pointer to an instance of an object to a static
	// member function, but we can't use a normal member function as a curl progress callback,
	// and we want access to some private members of Mootcher.

	Mootcher *thisMootcher = (Mootcher *) caller;

	if (thisMootcher->cancel_download) {
		return -1;
	}

	thisMootcher->Progress(dlnow, dltotal); /* EMIT SIGNAL */
	return 0;
}

