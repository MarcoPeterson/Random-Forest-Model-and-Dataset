################################################################ Getting the Data and Data Pre-processing for various Models ########################################################
## Load libraries for model

library("NLP")
library("glue")
library("readr")
library("dplyr")
library("tidytext")
library("textdata")
library("stringr")
library("tm")
library("wordcloud")
library("syuzhet")
library("tibble")
library("tidyverse")
library("tm")
library("widyr")
library("ggplot2")
library("tidyr")
library("igraph")
library("ggraph")
library("stringr")
library("gender")
library("genderdata")
library("reshape2")
library("qdapDictionaries")
library("splitstackshape")
library("DBI")
library("plyr")


## Connect to Snowflake

myconn <- DBI::dbConnect(odbc::odbc(), "Snowflake", uid="MPETERSON", pwd='Welcome1@') 
warehouse <- DBI::dbGetQuery(myconn,"USE WAREHOUSE PROD_MGMT_XS")

## Original dataset

case_data_df <- DBI::dbGetQuery(myconn,
                                "
select *
FROM PROD_MGMT_DB.CASE_DATA.CASE_FEATURE_DATA_ALL
WHERE SALESFORCE_CASE_CREATED_DATE >= '2021-01-01'
")

############################################################################### Email subsection ####################################################################################

## Start off the script with running the stop words specific for the Email subsection


email_stopwords = c("Cx", "cx", "calling in", "Hello", "hello", "cradlepoint", "Cradlepoint", "Device", "device", "case", "Case", "Please", "please", "Attached", "attach",
"called", "Client", "client", "Case", "case", "Caller", "www", "CC", "cc", "com", "PM", "FA", "MAC", "Chat", "SN", "NCOS", "Chat started by",
"Thank you for contacting", "To better assist you", "provide the following information", "Current NCOS Version", "http", "Device Information", "ab", "P", "Carrier",
"K", "C", "AM", "J", "GED", "E", "A", "B", "C", "D", "E", "F", "G", "H", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
"WA", "MM", "CBA", "NOCOS", "IBR", "Serial", "TB", "CM" ,"NNN", "IBRC", "CBALP", "AER", "DA", "WARA", "AE", "BA", "BB", "AC", "AT", "BODY",
"NetCloud Engine","IBRC","AER","IBR","MANAGED SERVICES","CTR","MBR","Cradlepoint Connect","End of Support","Product Recommendation","W","R PLTE",
"MCLPE VZ","MC","CBR","E","IBR FIPS","CBA","Accessories","L","CX","IBRB","CR","CBAB","AER FIPS","MBRB","MCLPE AT","Cradlepoint University","MBRv",
"CradleCare","NetCloud Manager","AP","CR PoE","MCLPE GN","Beta","R","Cradlepoint Virtual Router", "VZW", "MCLP", "https", "Sent from my iPhone", "jpg")

## gender names that are in the kantrowitz dictionary

email_stopwords2 <- c(kantrowitz$name)

## Run preliminary code

email_subset <- tibble(case_data_df)
email_subset <- email_subset %>% filter(SALESFORCE_CASE_ORIGIN == "Email")
email_subset$SALESFORCE_CASE_PRODUCT_C <- as.factor(email_subset$SALESFORCE_CASE_PRODUCT_C)
email_subset$SALESFORCE_CASE_DESCRIPTION <- email_subset$SALESFORCE_CASE_DESCRIPTION %>% replace_na("Unknown")
email_subset$SALESFORCE_CASE_DESCRIPTION <- str_replace_all(email_subset$SALESFORCE_CASE_DESCRIPTION, "[^[:alnum:]]", " ")
email_subset$SALESFORCE_CASE_DESCRIPTION <- gsub('[[:digit:]]+', '', email_subset$SALESFORCE_CASE_DESCRIPTION)
# email_subset$SALESFORCE_CASE_DESCRIPTION <- gsub("\\b[[:alpha:]]{16,}\\b", "", email_subset$SALESFORCE_CASE_DESCRIPTION, perl=T)


## This is to get the words from the GradyAugmented dictionary and transform it as a dataframe

email_stopwords3 <- as.data.frame(GradyAugmented)

## These are words you want to add to the dictionary, before they are removed

colnames(email_stopwords3) <- c("word")

added_words <- rbind("ncm", "internet", "admin", "Admin", "amazonaws", "app", "apps", "att", "ATT", "authenticator", "Authenticator", "	AuthorizationError",
                     "blog", "Blog", "Boise", "CEO", "comcast", "config", "Config", "connectivity", "Connectivity", "controlware", "Controlware", "cradlepointcm",
                     "cradlepointecm", "CradlepointECM", "encryption", "Encryption", "endpoint", "Endpoint", "Firefox", "firewall", "firmware", "inbox", "internet", "Internet",
                     "jpg", "login", "Login", "logins", "Logins", "logoff", "malware", "ncm", "NCM", "netcloud", "Netcloud", "NetCloud", "NetCLoud", "NetClound", "Netcoud",
                     "Netcould", "onsite", "passcode", "Passthrough", "pw", "PW", "pwd", "screenshot", "servicedesk", "ServiceDesk", "servicessupport", "ServicesSupport",
                     "sku", "Sku","Skype","smartphone", "Smartphone", "sourcepage", "spam", "spams", "SPAM", "sysadmin", "Sysadmin", "wifi", "Wifi", "WIFI","youtube", "Youtube",
                     "YouTube", "analytics", "Analytics", "api", "API", "AutoAPN", "AWS", "broadband", "download", "Download", "downloads", "dropdown", "endpoints",
                     "html", "http", "https", "IOS", "iOS", "IoT", "iot", "IOT", "Iot", "IP", "ip", "iP", "Ip", "Modem", "MODEM", "modems", "Modems", "nectcloud",
                     "Speedtest", "speedtest", "speedtests", "spreadsheet", "spreadsheets", "Tmobile", "tmobile", "TMobile","unregister", "Unregister", "unregistered",
                     "unregistering", "upload", "Upload", "URL", "url", "Verizon", "verizon", "zscaler", "Zscaler", "Ericsson", "email", "Email", "failover", "Failover",
                     "offline", "Offline", "OFFLINE", "Okta", "OKTA","online", "Online","upload", "Upload", "uploaded", "autovpn", "AutoVPN", "autoVPN", "failover", "failovers",
                     "iOs", "IoT", "IOT", "LAN", "lan", "laptop", "Laptop", "laptops", "linux", "Linux", "netgear", "Netgear", "offline", "Offline", "Online", "Online",
                     "Okta", "okta", "reboot", "rebooting", "Rebooted", "Rebooting", "speedtest", "Speedtest", "subaccount", "Subaccount", "cradlepoint", "Cradlepoint",
                     "ethernet", "checkbox", "Checkbox", "deactivated", "uninstall", "uploads", "workflow", 
                     "admins", "Admins", "LinkedIn", "logon", "logout", "NetCIoud", "NetCloudManager", "Fedex", "FedEx", "fedex", "VPN", "vpn", "VPNs", "vpns", "CloudManager", "LANs", "subscriptons", "APN", "apn", "APNs", "firewalls",
                     "IPSEC", "IPSec", "ipsec", "IPsec", "Ipsec", "SSID", "failback", "icloud", "IPN", "NCOS", "OpenDNS", "openVPN", "OpenVPN", "SSID", "ssid", "SSId", "SSIDs", "ssids", "urls", "URLs", "username", "Username")

colnames(added_words) <- c("word")
added_words <- as.data.frame(added_words)

## This is to bind the dataframes

email_stopwords3 <-rbind(email_stopwords3, added_words)

## transform the dataframe list back to an unlisted vector

email_stopwords3 <- as.vector(unlist(email_stopwords3$word))

df <- removeWords(email_subset$SALESFORCE_CASE_DESCRIPTION, email_stopwords3)
df <- as.data.frame(df)
colnames(df) <- c("word")

## Splits concatenated data of a string into seperate values as a cell in the same column

split_data <- cSplit(df, "word", sep = " ", direction = "long")

## Transforms the data into unique

split_data <- unique(split_data)
split_data <- as.data.frame(split_data)

## This is the final version

email_stopwords3 <- as.vector(unlist(split_data$word))

## Save email removed words for later

email_removed_words <- as.data.frame(unlist(split_data$word))
colnames(email_removed_words) <- c("email_removed_words")

## This is to complete the Email subsection

uncleansed_email_description_subset <- case_data_df %>% filter(SALESFORCE_CASE_ORIGIN == "Email")
email_subset$SALESFORCE_CASE_DESCRIPTION_Uncleansed <- uncleansed_email_description_subset$SALESFORCE_CASE_DESCRIPTION
email_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(email_subset$SALESFORCE_CASE_DESCRIPTION, email_stopwords)
email_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(email_subset$SALESFORCE_CASE_DESCRIPTION, email_stopwords2)
email_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(email_subset$SALESFORCE_CASE_DESCRIPTION, email_stopwords3)
email_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(email_subset$SALESFORCE_CASE_DESCRIPTION, stopwords("english"))
email_subset$SALESFORCE_CASE_DESCRIPTION <- str_squish(email_subset$SALESFORCE_CASE_DESCRIPTION)


############################################################################## Chat subsection ######################################################################################

## Start off the script with running the stop words specific for the Chat subsection


chat_stopwords = c("Cx", "cx", "calling in", "Hello", "hello", "cradlepoint", "Cradlepoint", "Device", "device", "case", "Case", "Please", "please", "Attached", "attach",
              "called", "Client", "client", "Case", "case", "Caller", "www", "CC", "cc", "com", "PM", "FA", "MAC", "Chat", "SN", "NCOS", "Chat started by",
              "Thank you for contacting", "To better assist you", "provide the following information", "Current NCOS Version", "http", "Device Information", "ab", "P", "Carrier",
              "K", "C", "AM", "J", "GED", "E", "A", "B", "C", "D", "E", "F", "G", "H", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
              "WA", "MM", "CBA", "NOCOS", "IBR", "Serial", "TB", "CM" ,"NNN", "IBRC", "CBALP", "AER", "DA", "WARA", "AE", "BA", "BB", "AC", "AT", "BODY",
              "NetCloud Engine","IBRC","AER","IBR","MANAGED SERVICES","CTR","MBR","Cradlepoint Connect","End of Support","Product Recommendation","W","R PLTE",
              "MCLPE VZ","MC","CBR","E","IBR FIPS","CBA","Accessories","L","CX","IBRB","CR","CBAB","AER FIPS","MBRB","MCLPE AT","Cradlepoint University","MBRv",
              "CradleCare","AP","CR PoE","MCLPE GN","Beta","R","Cradlepoint Virtual Router", "VZW", "MCLP", "https", "Sent from my iPhone", "jpg")

## gender names that are in the kantrowitz dictionary

chat_stopwords2 <- c(kantrowitz$name)


## This is the Chat subsection

chat_subset <- tibble(case_data_df)
chat_subset <- chat_subset %>% filter(SALESFORCE_CASE_ORIGIN == "Chat")
chat_subset$SALESFORCE_CASE_PRODUCT_C <- as.factor(chat_subset$SALESFORCE_CASE_PRODUCT_C)
chat_subset$SALESFORCE_CASE_DESCRIPTION <- chat_subset$SALESFORCE_CASE_DESCRIPTION %>% replace_na("Unknown")
chat_subset$SALESFORCE_CASE_DESCRIPTION <- str_replace_all(chat_subset$SALESFORCE_CASE_DESCRIPTION, "[^[:alnum:]]", " ")
chat_subset$SALESFORCE_CASE_DESCRIPTION <- gsub('[[:digit:]]+', '', chat_subset$SALESFORCE_CASE_DESCRIPTION)


## This is to get the words from the GradyAugmented dictionary and transform it as a dataframe

chat_stopwords3 <- as.data.frame(GradyAugmented)

## These are words you want to add to the dictionary, before they are removed

colnames(chat_stopwords3) <- c("word")

added_words <- rbind("ncm", "internet", "admin", "Admin", "amazonaws", "app", "apps", "att", "ATT", "authenticator", "Authenticator", "	AuthorizationError",
                     "blog", "Blog", "Boise", "CEO", "comcast", "config", "Config", "connectivity", "Connectivity", "controlware", "Controlware", "cradlepointcm",
                     "cradlepointecm", "CradlepointECM", "encryption", "Encryption", "endpoint", "Endpoint", "Firefox", "firewall", "firmware", "inbox", "internet", "Internet",
                     "jpg", "login", "Login", "logins", "Logins", "logoff", "malware", "ncm", "NCM", "netcloud", "Netcloud", "NetCloud", "NetCLoud", "NetClound", "Netcoud",
                     "Netcould", "onsite", "passcode", "Passthrough", "pw", "PW", "pwd", "screenshot", "servicedesk", "ServiceDesk", "servicessupport", "ServicesSupport",
                     "sku", "Sku","Skype","smartphone", "Smartphone", "sourcepage", "spam", "spams", "SPAM", "sysadmin", "Sysadmin", "wifi", "Wifi", "WIFI","youtube", "Youtube",
                     "YouTube", "analytics", "Analytics", "api", "API", "AutoAPN", "AWS", "broadband", "download", "Download", "downloads", "dropdown", "endpoints",
                     "html", "http", "https", "IOS", "iOS", "IoT", "iot", "IOT", "Iot", "IP", "ip", "iP", "Ip", "Modem", "MODEM", "modems", "Modems", "nectcloud",
                     "Speedtest", "speedtest", "speedtests", "spreadsheet", "spreadsheets", "Tmobile", "tmobile", "TMobile","unregister", "Unregister", "unregistered",
                     "unregistering", "upload", "Upload", "URL", "url", "Verizon", "verizon", "zscaler", "Zscaler", "Ericsson", "email", "Email", "failover", "Failover",
                     "offline", "Offline", "OFFLINE", "Okta", "OKTA","online", "Online","upload", "Upload", "uploaded", "autovpn", "AutoVPN", "autoVPN", "failover", "failovers",
                     "iOs", "IoT", "IOT", "LAN", "lan", "laptop", "Laptop", "laptops", "linux", "Linux", "netgear", "Netgear", "offline", "Offline", "Online", "Online",
                     "Okta", "okta", "reboot", "rebooting", "Rebooted", "Rebooting", "speedtest", "Speedtest", "subaccount", "Subaccount", "cradlepoint", "Cradlepoint",
                     "ethernet", "checkbox", "Checkbox", "deactivated", "uninstall", "uploads", "workflow", "CloudManager", "deregistered",
                     "admins", "Admins", "LinkedIn", "logon", "logout", "NetCIoud", "NetCloudManager", "Fedex", "FedEx", "fedex", "VPN", "vpn", "VPNs", "vpns", "CloudManager", "LANs", "subscriptons", "APN", "apn", "APNs", "firewalls",
                     "IPSEC", "IPSec", "ipsec", "IPsec", "Ipsec", "SSID", "failback", "icloud", "IPN", "NCOS", "OpenDNS", "openVPN", "OpenVPN", "SSID", "ssid", "SSId", "SSIDs", "ssids", "urls", "URLs", "username", "Username")

colnames(added_words) <- c("word")

added_words <- as.data.frame(added_words)

## This is to bind the dataframes

chat_stopwords3 <-rbind(chat_stopwords3, added_words)

## transform the dataframe list back to an unlisted vector

chat_stopwords3 <- as.vector(unlist(chat_stopwords3$word))


df <- removeWords(chat_subset$SALESFORCE_CASE_DESCRIPTION, chat_stopwords3)
df <- as.data.frame(df)
colnames(df) <- c("word")

## Splits concatenated data of a string into seperate values as a cell in the same column

split_data <- cSplit(df, "word", sep = " ", direction = "long")

## Transforms the data into unique

split_data <- unique(split_data)
split_data <- as.data.frame(split_data)

## This is the final version

chat_stopwords3 <- as.vector(unlist(split_data$word))


## Save chat removed words for later

chat_removed_words <- as.data.frame(unlist(split_data$word))
colnames(chat_removed_words) <- c("chat_removed_words")

## This is to complete the Chat subsection

uncleansed_chat_description_subset <- case_data_df %>% filter(SALESFORCE_CASE_ORIGIN == "Chat")
chat_subset$SALESFORCE_CASE_DESCRIPTION_Uncleansed <- uncleansed_chat_description_subset$SALESFORCE_CASE_DESCRIPTION
chat_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(chat_subset$SALESFORCE_CASE_DESCRIPTION, chat_stopwords)
chat_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(chat_subset$SALESFORCE_CASE_DESCRIPTION, chat_stopwords2)
chat_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(chat_subset$SALESFORCE_CASE_DESCRIPTION, chat_stopwords3)
chat_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(chat_subset$SALESFORCE_CASE_DESCRIPTION, stopwords("english"))
chat_subset$SALESFORCE_CASE_DESCRIPTION <- str_squish(chat_subset$SALESFORCE_CASE_DESCRIPTION)


############################################################################## Forum subsection #####################################################################################

## This is the Forum subsection

## Start off the script with running the stop words specific for the Forum subsection

## Selected stop words to remove

forum_stopwords = c("Cx", "cx", "calling in", "Hello", "hello", "cradlepoint", "Cradlepoint", "Device", "device", "case", "Case", "Please", "please", "Attached", "attach",
"called", "Client", "client", "Case", "case", "Caller", "www", "CC", "cc", "com", "PM", "FA", "MAC", "Chat", "SN", "NCOS", "Chat started by",
"Thank you for contacting", "To better assist you", "provide the following information", "Current NCOS Version", "http", "Device Information", "ab", "P", "Carrier",
"K", "C", "AM", "J", "GED", "E", "A", "B", "C", "D", "E", "F", "G", "H", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
"WA", "MM", "CBA", "NOCOS", "IBR", "Serial", "TB", "CM" ,"NNN", "IBRC", "CBALP", "AER", "DA", "WARA", "AE", "BA", "BB", "AC", "AT", "BODY",
"NetCloud Engine","IBRC","AER","IBR","MANAGED SERVICES","CTR","MBR","Cradlepoint Connect","End of Support","Product Recommendation","W","R PLTE",
"MCLPE VZ","MC","CBR","E","IBR FIPS","CBA","Accessories","L","CX","IBRB","CR","CBAB","AER FIPS","MBRB","MCLPE AT","Cradlepoint University","MBRv",
"CradleCare","NetCloud Manager","AP","CR PoE","MCLPE GN","Beta","R","Cradlepoint Virtual Router", "VZW", "MCLP", "https", "Sent from my iPhone", "jpg")

## gender names that are in the kantrowitz dictionary

forum_stopwords2 <- c(kantrowitz$name)

forum_subset <- tibble(case_data_df)
forum_subset <- forum_subset %>% filter(SALESFORCE_CASE_ORIGIN == "Forum")
forum_subset$SALESFORCE_CASE_PRODUCT_C <- as.factor(forum_subset$SALESFORCE_CASE_PRODUCT_C)
forum_subset$SALESFORCE_CASE_DESCRIPTION <- forum_subset$SALESFORCE_CASE_DESCRIPTION %>% replace_na("Unknown")
forum_subset$SALESFORCE_CASE_DESCRIPTION <- str_replace_all(forum_subset$SALESFORCE_CASE_DESCRIPTION, "[^[:alnum:]]", " ")
forum_subset$SALESFORCE_CASE_DESCRIPTION <- gsub('[[:digit:]]+', '', forum_subset$SALESFORCE_CASE_DESCRIPTION)
forum_stopwords3 <- as.data.frame(GradyAugmented)

## These are words you want to add to the dictionary, before they are removed

colnames(forum_stopwords3) <- c("word")

added_words <- rbind("ncm", "internet", "admin", "Admin", "amazonaws", "app", "apps", "att", "ATT", "authenticator", "Authenticator", "	AuthorizationError",
                     "blog", "Blog", "Boise", "CEO", "comcast", "config", "Config", "connectivity", "Connectivity", "controlware", "Controlware", "cradlepointcm",
                     "cradlepointecm", "CradlepointECM", "encryption", "Encryption", "endpoint", "Endpoint", "Firefox", "firewall", "firmware", "inbox", "internet", "Internet",
                     "jpg", "login", "Login", "logins", "Logins", "logoff", "malware", "ncm", "NCM", "netcloud", "Netcloud", "NetCloud", "NetCLoud", "NetClound", "Netcoud",
                     "Netcould", "onsite", "passcode", "Passthrough", "pw", "PW", "pwd", "screenshot", "servicedesk", "ServiceDesk", "servicessupport", "ServicesSupport",
                     "sku", "Sku","Skype","smartphone", "Smartphone", "sourcepage", "spam", "spams", "SPAM", "sysadmin", "Sysadmin", "wifi", "Wifi", "WIFI","youtube", "Youtube",
                     "YouTube", "analytics", "Analytics", "api", "API", "AutoAPN", "AWS", "broadband", "download", "Download", "downloads", "dropdown", "endpoints",
                     "html", "http", "https", "IOS", "iOS", "IoT", "iot", "IOT", "Iot", "IP", "ip", "iP", "Ip", "Modem", "MODEM", "modems", "Modems", "nectcloud",
                     "Speedtest", "speedtest", "speedtests", "spreadsheet", "spreadsheets", "Tmobile", "tmobile", "TMobile","unregister", "Unregister", "unregistered",
                     "unregistering", "upload", "Upload", "URL", "url", "Verizon", "verizon", "zscaler", "Zscaler", "Ericsson", "email", "Email", "failover", "Failover",
                     "offline", "Offline", "OFFLINE", "Okta", "OKTA","online", "Online","upload", "Upload", "uploaded", "autovpn", "AutoVPN", "autoVPN", "failover", "failovers",
                     "iOs", "IoT", "IOT", "LAN", "lan", "laptop", "Laptop", "laptops", "linux", "Linux", "netgear", "Netgear", "offline", "Offline", "Online", "Online",
                     "Okta", "okta", "reboot", "rebooting", "Rebooted", "Rebooting", "speedtest", "Speedtest", "subaccount", "Subaccount", "cradlepoint", "Cradlepoint",
                     "ethernet", "checkbox", "Checkbox", "deactivated", "uninstall", "uploads", "workflow",
                     "admins", "Admins", "LinkedIn", "logon", "logout", "NetCIoud", "NetCloudManager", "Fedex", "FedEx", "fedex", "VPN", "vpn", "VPNs", "vpns", "CloudManager", "LANs", "subscriptons")

colnames(added_words) <- c("word")

added_words <- as.data.frame(added_words)

## This is to bind the dataframes

forum_stopwords3 <-rbind(forum_stopwords3, added_words)

## transform the dataframe list back to an unlisted vector

forum_stopwords3 <- as.vector(unlist(forum_stopwords3$word))
df <- removeWords(forum_subset$SALESFORCE_CASE_DESCRIPTION, forum_stopwords3)
df <- as.data.frame(df)
colnames(df) <- c("word")

## Splits concatenated data of a string into seperate values as a cell in the same column

split_data <- cSplit(df, "word", sep = " ", direction = "long")

## Transforms the data into unique

split_data <- unique(split_data)
split_data <- as.data.frame(split_data)

## This is the final version

forum_stopwords3 <- as.vector(unlist(split_data$word))

## Save forum removed words for later

forum_removed_words <- as.data.frame(unlist(split_data$word))
colnames(forum_removed_words) <- c("forum_removed_words")


## This is to complete the Forum subsection


uncleansed_forum_description_subset <- case_data_df %>% filter(SALESFORCE_CASE_ORIGIN == "Forum")
forum_subset$SALESFORCE_CASE_DESCRIPTION_Uncleansed <- uncleansed_forum_description_subset$SALESFORCE_CASE_DESCRIPTION
forum_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(forum_subset$SALESFORCE_CASE_DESCRIPTION, forum_stopwords)
forum_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(forum_subset$SALESFORCE_CASE_DESCRIPTION, forum_stopwords2)
forum_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(forum_subset$SALESFORCE_CASE_DESCRIPTION, forum_stopwords3)
forum_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(forum_subset$SALESFORCE_CASE_DESCRIPTION, stopwords("english"))
forum_subset$SALESFORCE_CASE_DESCRIPTION <- str_squish(forum_subset$SALESFORCE_CASE_DESCRIPTION)


############################################################################## Portal subsection ####################################################################################

## This is the Portal sub-section

## Start off the script with running the stop words specific for the Portal subsection


portal_stopwords = c("Cx", "cx", "calling in", "Hello", "hello", "cradlepoint", "Cradlepoint", "Device", "device", "case", "Case", "Please", "please", "Attached", "attach",
              "called", "Client", "client", "Case", "case", "Caller", "www", "CC", "cc", "com", "PM", "FA", "MAC", "Chat", "SN", "NCOS", "Chat started by",
              "Thank you for contacting", "To better assist you", "provide the following information", "Current NCOS Version", "http", "Device Information", "ab", "P", "Carrier",
              "K", "C", "AM", "J", "GED", "E", "A", "B", "C", "D", "E", "F", "G", "H", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
              "WA", "MM", "CBA", "NOCOS", "IBR", "Serial", "TB", "CM" ,"NNN", "IBRC", "CBALP", "AER", "DA", "WARA", "AE", "BA", "BB", "AC", "AT", "BODY",
              "NetCloud Engine","IBRC","AER","IBR","MANAGED SERVICES","CTR","MBR","Cradlepoint Connect","End of Support","Product Recommendation","W","R PLTE",
              "MCLPE VZ","MC","CBR","E","IBR FIPS","CBA","Accessories","L","CX","IBRB","CR","CBAB","AER FIPS","MBRB","MCLPE AT","Cradlepoint University","MBRv",
              "CradleCare","NetCloud Manager","AP","CR PoE","MCLPE GN","Beta","R","Cradlepoint Virtual Router", "VZW", "MCLP", "https", "Sent from my iPhone", "jpg")

## gender names that are in the kantrowitz dictionary

portal_stopwords2 <- c(kantrowitz$name)


portal_subset <- tibble(case_data_df)
portal_subset <- portal_subset %>% filter(SALESFORCE_CASE_ORIGIN == "Portal")
portal_subset$SALESFORCE_CASE_PRODUCT_C <- as.factor(portal_subset$SALESFORCE_CASE_PRODUCT_C)
portal_subset$SALESFORCE_CASE_DESCRIPTION <- portal_subset$SALESFORCE_CASE_DESCRIPTION %>% replace_na("Unknown")
portal_subset$SALESFORCE_CASE_DESCRIPTION <- str_replace_all(portal_subset$SALESFORCE_CASE_DESCRIPTION, "[^[:alnum:]]", " ")
portal_subset$SALESFORCE_CASE_DESCRIPTION <- gsub('[[:digit:]]+', '', portal_subset$SALESFORCE_CASE_DESCRIPTION)
## This is to get the words from the GradyAugmented dictionary and transform it as a dataframe

portal_stopwords3 <- as.data.frame(GradyAugmented)

## These are words you want to add to the dictionary, before they are removed

colnames(portal_stopwords3) <- c("word")

added_words <- rbind("ncm", "internet", "admin", "Admin", "amazonaws", "app", "apps", "att", "ATT", "authenticator", "Authenticator", "	AuthorizationError",
                     "blog", "Blog", "Boise", "CEO", "comcast", "config", "Config", "connectivity", "Connectivity", "controlware", "Controlware", "cradlepointcm",
                     "cradlepointecm", "CradlepointECM", "encryption", "Encryption", "endpoint", "Endpoint", "Firefox", "firewall", "firmware", "inbox", "internet", "Internet",
                     "jpg", "login", "Login", "logins", "Logins", "logoff", "malware", "ncm", "NCM", "netcloud", "Netcloud", "NetCloud", "NetCLoud", "NetClound", "Netcoud",
                     "Netcould", "onsite", "passcode", "Passthrough", "pw", "PW", "pwd", "screenshot", "servicedesk", "ServiceDesk", "servicessupport", "ServicesSupport",
                     "sku", "Sku","Skype","smartphone", "Smartphone", "sourcepage", "spam", "spams", "SPAM", "sysadmin", "Sysadmin", "wifi", "Wifi", "WIFI","youtube", "Youtube",
                     "YouTube", "analytics", "Analytics", "api", "API", "AutoAPN", "AWS", "broadband", "download", "Download", "downloads", "dropdown", "endpoints",
                     "html", "http", "https", "IOS", "iOS", "IoT", "iot", "IOT", "Iot", "IP", "ip", "iP", "Ip", "Modem", "MODEM", "modems", "Modems", "nectcloud",
                     "Speedtest", "speedtest", "speedtests", "spreadsheet", "spreadsheets", "Tmobile", "tmobile", "TMobile","unregister", "Unregister", "unregistered",
                     "unregistering", "upload", "Upload", "URL", "url", "Verizon", "verizon", "zscaler", "Zscaler", "Ericsson", "email", "Email", "failover", "Failover",
                     "offline", "Offline", "OFFLINE", "Okta", "OKTA","online", "Online","upload", "Upload", "uploaded", "autovpn", "AutoVPN", "autoVPN", "failover", "failovers",
                     "iOs", "IoT", "IOT", "LAN", "lan", "laptop", "Laptop", "laptops", "linux", "Linux", "netgear", "Netgear", "offline", "Offline", "Online", "Online",
                     "Okta", "okta", "reboot", "rebooting", "Rebooted", "Rebooting", "speedtest", "Speedtest", "subaccount", "Subaccount", "cradlepoint", "Cradlepoint",
                     "ethernet", "checkbox", "Checkbox", "deactivated", "uninstall", "uploads", "workflow",
                     "admins", "Admins", "LinkedIn", "logon", "logout", "NetCIoud", "NetCloudManager", "Fedex", "FedEx", "fedex", "VPN", "vpn", "VPNs", "vpns", "CloudManager", "LANs", "subscriptons", "APN", "apn", "APNs", "firewalls",
                     "IPSEC", "IPSec", "ipsec", "IPsec", "Ipsec", "SSID")

colnames(added_words) <- c("word")

added_words <- as.data.frame(added_words)

## This is to bind the dataframes

portal_stopwords3 <-rbind(portal_stopwords3, added_words)
## transform the dataframe list back to an unlisted vector
portal_stopwords3 <- as.vector(unlist(portal_stopwords3$word))

df <- removeWords(portal_subset$SALESFORCE_CASE_DESCRIPTION, portal_stopwords3)
df <- as.data.frame(df)
colnames(df) <- c("word")

## Splits concatenated data of a string into seperate values as a cell in the same column

split_data <- cSplit(df, "word", sep = " ", direction = "long")

## Transforms the data into unique

split_data <- unique(split_data)
split_data <- as.data.frame(split_data)

## This is the final version

portal_stopwords3 <- as.vector(unlist(split_data$word))

## Save portal removed words for later

portal_removed_words <- as.data.frame(unlist(split_data$word))
colnames(portal_removed_words) <- c("portal_removed_words")


## This is to complete the Portal subsection

uncleansed_portal_description_subset <- case_data_df %>% filter(SALESFORCE_CASE_ORIGIN == "Portal")
portal_subset$SALESFORCE_CASE_DESCRIPTION_Uncleansed <- uncleansed_portal_description_subset$SALESFORCE_CASE_DESCRIPTION
portal_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(portal_subset$SALESFORCE_CASE_DESCRIPTION, portal_stopwords)
portal_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(portal_subset$SALESFORCE_CASE_DESCRIPTION, portal_stopwords2)
portal_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(portal_subset$SALESFORCE_CASE_DESCRIPTION, portal_stopwords3)
portal_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(portal_subset$SALESFORCE_CASE_DESCRIPTION, stopwords("english"))
portal_subset$SALESFORCE_CASE_DESCRIPTION <- str_squish(portal_subset$SALESFORCE_CASE_DESCRIPTION)


################################################################################ Saas subsection ####################################################################################

## This is the Saas sub-section

## Start off the script with running the stop words specific for the Saas subsection


saas_stopwords = c("Cx", "cx", "calling in", "Hello", "hello", "cradlepoint", "Cradlepoint", "Device", "device", "case", "Case", "Please", "please", "Attached", "attach",
              "called", "Client", "client", "Case", "case", "Caller", "www", "CC", "cc", "com", "PM", "FA", "MAC", "Chat", "SN", "NCOS", "Chat started by",
              "Thank you for contacting", "To better assist you", "provide the following information", "Current NCOS Version", "http", "Device Information", "ab", "P", "Carrier",
              "K", "C", "AM", "J", "GED", "E", "A", "B", "C", "D", "E", "F", "G", "H", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
              "WA", "MM", "CBA", "NOCOS", "IBR", "Serial", "TB", "CM" ,"NNN", "IBRC", "CBALP", "AER", "DA", "WARA", "AE", "BA", "BB", "AC", "AT", "BODY",
              "NetCloud Engine","IBRC","AER","IBR","MANAGED SERVICES","CTR","MBR","Cradlepoint Connect","End of Support","Product Recommendation","W","R PLTE",
              "MCLPE VZ","MC","CBR","E","IBR FIPS","CBA","Accessories","L","CX","IBRB","CR","CBAB","AER FIPS","MBRB","MCLPE AT","Cradlepoint University","MBRv",
              "CradleCare","NetCloud Manager","AP","CR PoE","MCLPE GN","Beta","R","Cradlepoint Virtual Router", "VZW", "MCLP", "https", "Sent from my iPhone", "jpg")

## gender names that are in the kantrowitz dictionary

saas_stopwords2 <- c(kantrowitz$name)


saas_subset <- tibble(case_data_df)
saas_subset <- saas_subset %>% filter(SALESFORCE_CASE_ORIGIN == "SaaS")
saas_subset$SALESFORCE_CASE_PRODUCT_C <- as.factor(saas_subset$SALESFORCE_CASE_PRODUCT_C)
saas_subset$SALESFORCE_CASE_DESCRIPTION <- saas_subset$SALESFORCE_CASE_DESCRIPTION %>% replace_na("Unknown")
saas_subset$SALESFORCE_CASE_DESCRIPTION <- str_replace_all(saas_subset$SALESFORCE_CASE_DESCRIPTION, "[^[:alnum:]]", " ")
saas_subset$SALESFORCE_CASE_DESCRIPTION <- gsub('[[:digit:]]+', '', saas_subset$SALESFORCE_CASE_DESCRIPTION)
## This is to get the words from the GradyAugmented dictionary and transform it as a dataframe

saas_stopwords3 <- as.data.frame(GradyAugmented)

## These are words you want to add to the dictionary, before they are removed

colnames(saas_stopwords3) <- c("word")

added_words <- rbind("ncm", "internet", "admin", "Admin", "amazonaws", "app", "apps", "att", "ATT", "authenticator", "Authenticator", "	AuthorizationError",
                     "blog", "Blog", "Boise", "CEO", "comcast", "config", "Config", "connectivity", "Connectivity", "controlware", "Controlware", "cradlepointcm",
                     "cradlepointecm", "CradlepointECM", "encryption", "Encryption", "endpoint", "Endpoint", "Firefox", "firewall", "firmware", "inbox", "internet", "Internet",
                     "jpg", "login", "Login", "logins", "Logins", "logoff", "malware", "ncm", "NCM", "netcloud", "Netcloud", "NetCloud", "NetCLoud", "NetClound", "Netcoud",
                     "Netcould", "onsite", "passcode", "Passthrough", "pw", "PW", "pwd", "screenshot", "servicedesk", "ServiceDesk", "servicessupport", "ServicesSupport",
                     "sku", "Sku","Skype","smartphone", "Smartphone", "sourcepage", "spam", "spams", "SPAM", "sysadmin", "Sysadmin", "wifi", "Wifi", "WIFI","youtube", "Youtube",
                     "YouTube", "analytics", "Analytics", "api", "API", "AutoAPN", "AWS", "broadband", "download", "Download", "downloads", "dropdown", "endpoints",
                     "html", "http", "https", "IOS", "iOS", "IoT", "iot", "IOT", "Iot", "IP", "ip", "iP", "Ip", "Modem", "MODEM", "modems", "Modems", "nectcloud",
                     "Speedtest", "speedtest", "speedtests", "spreadsheet", "spreadsheets", "Tmobile", "tmobile", "TMobile","unregister", "Unregister", "unregistered",
                     "unregistering", "upload", "Upload", "URL", "url", "Verizon", "verizon", "zscaler", "Zscaler", "Ericsson", "email", "Email", "failover", "Failover",
                     "offline", "Offline", "OFFLINE", "Okta", "OKTA","online", "Online","upload", "Upload", "uploaded", "autovpn", "AutoVPN", "autoVPN", "failover", "failovers",
                     "iOs", "IoT", "IOT", "LAN", "lan", "laptop", "Laptop", "laptops", "linux", "Linux", "netgear", "Netgear", "offline", "Offline", "Online", "Online",
                     "Okta", "okta", "reboot", "rebooting", "Rebooted", "Rebooting", "speedtest", "Speedtest", "subaccount", "Subaccount", "cradlepoint", "Cradlepoint",
                     "ethernet", "checkbox", "Checkbox", "deactivated", "uninstall", "uploads", "workflow")

colnames(added_words) <- c("word")

added_words <- as.data.frame(added_words)

## This is to bind the dataframes

saas_stopwords3 <-rbind(saas_stopwords3, added_words)
## transform the dataframe list back to an unlisted vector
saas_stopwords3 <- as.vector(unlist(saas_stopwords3$word))

df <- removeWords(saas_subset$SALESFORCE_CASE_DESCRIPTION, saas_stopwords3)
df <- as.data.frame(df)
colnames(df) <- c("word")

## Splits concatenated data of a string into seperate values as a cell in the same column

split_data <- cSplit(df, "word", sep = " ", direction = "long")

## Transforms the data into unique

split_data <- unique(split_data)
split_data <- as.data.frame(split_data)

## This is the final version

saas_stopwords3 <- as.vector(unlist(split_data$word))

## Save portal removed words for later

saas_removed_words <- as.data.frame(unlist(split_data$word))
colnames(saas_removed_words) <- c("saas_removed_words")


## This is to complete the Saas subsection

uncleansed_SaaS_description_subset <- case_data_df %>% filter(SALESFORCE_CASE_ORIGIN == "SaaS")
saas_subset$SALESFORCE_CASE_DESCRIPTION_Uncleansed <- uncleansed_SaaS_description_subset$SALESFORCE_CASE_DESCRIPTION
saas_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(saas_subset$SALESFORCE_CASE_DESCRIPTION, saas_stopwords)
saas_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(saas_subset$SALESFORCE_CASE_DESCRIPTION, saas_stopwords2)
saas_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(saas_subset$SALESFORCE_CASE_DESCRIPTION, saas_stopwords3)
saas_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(saas_subset$SALESFORCE_CASE_DESCRIPTION, stopwords("english"))
saas_subset$SALESFORCE_CASE_DESCRIPTION <- str_squish(saas_subset$SALESFORCE_CASE_DESCRIPTION)

############################################################################### Phone subsection ####################################################################################

## Start off the script with running the stop words specific for the Phone subsection


phone_stopwords = c("Cx", "cx", "calling in", "Hello", "hello", "cradlepoint", "Cradlepoint", "Device", "device", "case", "Case", "Please", "please", "Attached", "attach",
              "called", "Client", "client", "Case", "case", "Caller", "www", "CC", "cc", "com", "PM", "FA", "MAC", "Chat", "SN", "NCOS", "Chat started by",
              "Thank you for contacting", "To better assist you", "provide the following information", "Current NCOS Version", "http", "Device Information", "ab", "P", "Carrier",
              "K", "C", "AM", "J", "GED", "E", "A", "B", "C", "D", "E", "F", "G", "H", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
              "WA", "MM", "CBA", "NOCOS", "IBR", "Serial", "TB", "CM" ,"NNN", "IBRC", "CBALP", "AER", "DA", "WARA", "AE", "BA", "BB", "AC", "AT", "BODY",
              "NetCloud Engine","IBRC","AER","IBR","MANAGED SERVICES","CTR","MBR","Cradlepoint Connect","End of Support","Product Recommendation","W","R PLTE",
              "MCLPE VZ","MC","CBR","E","IBR FIPS","CBA","Accessories","L","CX","IBRB","CR","CBAB","AER FIPS","MBRB","MCLPE AT","Cradlepoint University","MBRv",
              "CradleCare","NetCloud Manager","AP","CR PoE","MCLPE GN","Beta","R","Cradlepoint Virtual Router", "VZW", "MCLP", "https", "Sent from my iPhone", "jpg")

## gender names that are in the kantrowitz dictionary

phone_stopwords2 <- c(kantrowitz$name)


phone_subset <- tibble(case_data_df)
phone_subset <- phone_subset %>% filter(SALESFORCE_CASE_ORIGIN == "Phone")
phone_subset$SALESFORCE_CASE_PRODUCT_C <- as.factor(phone_subset$SALESFORCE_CASE_PRODUCT_C)
phone_subset$SALESFORCE_CASE_DESCRIPTION <- phone_subset$SALESFORCE_CASE_DESCRIPTION %>% replace_na("Unknown")
phone_subset$SALESFORCE_CASE_DESCRIPTION <- str_replace_all(phone_subset$SALESFORCE_CASE_DESCRIPTION, "[^[:alnum:]]", " ")
phone_subset$SALESFORCE_CASE_DESCRIPTION <- gsub('[[:digit:]]+', '', phone_subset$SALESFORCE_CASE_DESCRIPTION)
## This is to get the words from the GradyAugmented dictionary and transform it as a dataframe

phone_stopwords3 <- as.data.frame(GradyAugmented)

## These are words you want to add to the dictionary, before they are removed

colnames(phone_stopwords3) <- c("word")
added_words <- rbind("ncm", "internet", "admin", "Admin", "amazonaws", "app", "apps", "att", "ATT", "authenticator", "Authenticator", "	AuthorizationError",
                     "blog", "Blog", "Boise", "CEO", "comcast", "config", "Config", "connectivity", "Connectivity", "controlware", "Controlware", "cradlepointcm",
                     "cradlepointecm", "CradlepointECM", "encryption", "Encryption", "endpoint", "Endpoint", "Firefox", "firewall", "firmware", "inbox", "internet", "Internet",
                     "jpg", "login", "Login", "logins", "Logins", "logoff", "malware", "ncm", "NCM", "netcloud", "Netcloud", "NetCloud", "NetCLoud", "NetClound", "Netcoud",
                     "Netcould", "onsite", "passcode", "Passthrough", "pw", "PW", "pwd", "screenshot", "servicedesk", "ServiceDesk", "servicessupport", "ServicesSupport",
                     "sku", "Sku","Skype","smartphone", "Smartphone", "sourcepage", "spam", "spams", "SPAM", "sysadmin", "Sysadmin", "wifi", "Wifi", "WIFI","youtube", "Youtube",
                     "YouTube", "analytics", "Analytics", "api", "API", "AutoAPN", "AWS", "broadband", "download", "Download", "downloads", "dropdown", "endpoints",
                     "html", "http", "https", "IOS", "iOS", "IoT", "iot", "IOT", "Iot", "IP", "ip", "iP", "Ip", "Modem", "MODEM", "modems", "Modems", "nectcloud",
                     "Speedtest", "speedtest", "speedtests", "spreadsheet", "spreadsheets", "Tmobile", "tmobile", "TMobile","unregister", "Unregister", "unregistered",
                     "unregistering", "upload", "Upload", "URL", "url", "Verizon", "verizon", "zscaler", "Zscaler", "Ericsson", "email", "Email", "failover", "Failover",
                     "offline", "Offline", "OFFLINE", "Okta", "OKTA","online", "Online","upload", "Upload", "uploaded", "autovpn", "AutoVPN", "autoVPN", "failover", "failovers",
                     "iOs", "IoT", "IOT", "LAN", "lan", "laptop", "Laptop", "laptops", "linux", "Linux", "netgear", "Netgear", "offline", "Offline", "Online", "Online",
                     "Okta", "okta", "reboot", "rebooting", "Rebooted", "Rebooting", "speedtest", "Speedtest", "subaccount", "Subaccount", "cradlepoint", "Cradlepoint",
                     "ethernet", "checkbox", "Checkbox", "deactivated", "uninstall", "uploads", "workflow",
                     "admins", "Admins", "LinkedIn", "logon", "logout", "NetCIoud", "NetCloudManager", "Fedex", "FedEx", "fedex", "VPN", "vpn", "VPNs", "vpns", "CloudManager", "LANs", "subscriptons", "APN", "apn", "APNs", "firewalls",
                     "IPSEC", "IPSec", "ipsec", "IPsec", "Ipsec", "SSID", "failback", "icloud", "IPN", "NCOS", "OpenDNS", "openVPN", "OpenVPN", "SSID", "ssid", "SSId", "SSIDs", "ssids", "urls", "URLs")

colnames(added_words) <- c("word")
added_words <- as.data.frame(added_words)

## This is to bind the dataframes

phone_stopwords3 <-rbind(phone_stopwords3, added_words)
## transform the dataframe list back to an unlisted vector
phone_stopwords3 <- as.vector(unlist(phone_stopwords3$word))

df <- removeWords(phone_subset$SALESFORCE_CASE_DESCRIPTION, phone_stopwords3)
df <- as.data.frame(df)
colnames(df) <- c("word")

## Splits concatenated data of a string into seperate values as a cell in the same column

split_data <- cSplit(df, "word", sep = " ", direction = "long")

## Transforms the data into unique

split_data <- unique(split_data)
split_data <- as.data.frame(split_data)

## This is the final version

phone_stopwords3 <- as.vector(unlist(split_data$word))


## Save portal removed words for later

phone_removed_words <- as.data.frame(unlist(split_data$word))
colnames(phone_removed_words) <- c("phone_removed_words")

## This is to complete the Phone subsection

uncleansed_phone_description_subset <- case_data_df %>% filter(SALESFORCE_CASE_ORIGIN == "Phone")
phone_subset$SALESFORCE_CASE_DESCRIPTION_Uncleansed <- uncleansed_phone_description_subset$SALESFORCE_CASE_DESCRIPTION
phone_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(phone_subset$SALESFORCE_CASE_DESCRIPTION, phone_stopwords)
phone_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(phone_subset$SALESFORCE_CASE_DESCRIPTION, phone_stopwords2)
phone_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(phone_subset$SALESFORCE_CASE_DESCRIPTION, phone_stopwords3)
phone_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(phone_subset$SALESFORCE_CASE_DESCRIPTION, stopwords("english"))
phone_subset$SALESFORCE_CASE_DESCRIPTION <- str_squish(phone_subset$SALESFORCE_CASE_DESCRIPTION)

#########################################################################  Partner Connect subsection ##############################################################################


## This is the Partner Connect sub-section

## Start off the script with running the stop words specific for the Partner Connect subsection


partner_connect_stopwords = c("Cx", "cx", "calling in", "Hello", "hello", "cradlepoint", "Cradlepoint", "Device", "device", "case", "Case", "Please", "please", "Attached", "attach",
              "called", "Client", "client", "Case", "case", "Caller", "www", "CC", "cc", "com", "PM", "FA", "MAC", "Chat", "SN", "NCOS", "Chat started by",
              "Thank you for contacting", "To better assist you", "provide the following information", "Current NCOS Version", "http", "Device Information", "ab", "P", "Carrier",
              "K", "C", "AM", "J", "GED", "E", "A", "B", "C", "D", "E", "F", "G", "H", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
              "WA", "MM", "CBA", "NOCOS", "IBR", "Serial", "TB", "CM" ,"NNN", "IBRC", "CBALP", "AER", "DA", "WARA", "AE", "BA", "BB", "AC", "AT", "BODY",
              "NetCloud Engine","IBRC","AER","IBR","MANAGED SERVICES","CTR","MBR","Cradlepoint Connect","End of Support","Product Recommendation","W","R PLTE",
              "MCLPE VZ","MC","CBR","E","IBR FIPS","CBA","Accessories","L","CX","IBRB","CR","CBAB","AER FIPS","MBRB","MCLPE AT","Cradlepoint University","MBRv",
              "CradleCare","NetCloud Manager","AP","CR PoE","MCLPE GN","Beta","R","Cradlepoint Virtual Router", "VZW", "MCLP", "https", "Sent from my iPhone", "jpg")

## gender names that are in the kantrowitz dictionary

partner_connect_stopwords2 <- c(kantrowitz$name)


partner_connect_subset <- tibble(case_data_df)
partner_connect_subset <- partner_connect_subset %>% filter(SALESFORCE_CASE_ORIGIN == "Partner Connect")
partner_connect_subset$SALESFORCE_CASE_PRODUCT_C <- as.factor(partner_connect_subset$SALESFORCE_CASE_PRODUCT_C)
partner_connect_subset$SALESFORCE_CASE_DESCRIPTION <- partner_connect_subset$SALESFORCE_CASE_DESCRIPTION %>% replace_na("Unknown")
partner_connect_subset$SALESFORCE_CASE_DESCRIPTION <- str_replace_all(partner_connect_subset$SALESFORCE_CASE_DESCRIPTION, "[^[:alnum:]]", " ")
partner_connect_subset$SALESFORCE_CASE_DESCRIPTION <- gsub('[[:digit:]]+', '', partner_connect_subset$SALESFORCE_CASE_DESCRIPTION)
## This is to get the words from the GradyAugmented dictionary and transform it as a dataframe

partner_connect_stopwords3 <- as.data.frame(GradyAugmented)

## These are words you want to add to the dictionary, before they are removed

colnames(partner_connect_stopwords3) <- c("word")
added_words <- rbind("ncm", "internet", "admin", "Admin", "amazonaws", "app", "apps", "att", "ATT", "authenticator", "Authenticator", "	AuthorizationError",
                     "blog", "Blog", "Boise", "CEO", "comcast", "config", "Config", "connectivity", "Connectivity", "controlware", "Controlware", "cradlepointcm",
                     "cradlepointecm", "CradlepointECM", "encryption", "Encryption", "endpoint", "Endpoint", "Firefox", "firewall", "firmware", "inbox", "internet", "Internet",
                     "jpg", "login", "Login", "logins", "Logins", "logoff", "malware", "ncm", "NCM", "netcloud", "Netcloud", "NetCloud", "NetCLoud", "NetClound", "Netcoud",
                     "Netcould", "onsite", "passcode", "Passthrough", "pw", "PW", "pwd", "screenshot", "servicedesk", "ServiceDesk", "servicessupport", "ServicesSupport",
                     "sku", "Sku","Skype","smartphone", "Smartphone", "sourcepage", "spam", "spams", "SPAM", "sysadmin", "Sysadmin", "wifi", "Wifi", "WIFI","youtube", "Youtube",
                     "YouTube", "analytics", "Analytics", "api", "API", "AutoAPN", "AWS", "broadband", "download", "Download", "downloads", "dropdown", "endpoints",
                     "html", "http", "https", "IOS", "iOS", "IoT", "iot", "IOT", "Iot", "IP", "ip", "iP", "Ip", "Modem", "MODEM", "modems", "Modems", "nectcloud",
                     "Speedtest", "speedtest", "speedtests", "spreadsheet", "spreadsheets", "Tmobile", "tmobile", "TMobile","unregister", "Unregister", "unregistered",
                     "unregistering", "upload", "Upload", "URL", "url", "Verizon", "verizon", "zscaler", "Zscaler", "Ericsson", "email", "Email", "failover", "Failover",
                     "offline", "Offline", "OFFLINE", "Okta", "OKTA","online", "Online","upload", "Upload", "uploaded", "autovpn", "AutoVPN", "autoVPN", "failover", "failovers",
                     "iOs", "IoT", "IOT", "LAN", "lan", "laptop", "Laptop", "laptops", "linux", "Linux", "netgear", "Netgear", "offline", "Offline", "Online", "Online",
                     "Okta", "okta", "reboot", "rebooting", "Rebooted", "Rebooting", "speedtest", "Speedtest", "subaccount", "Subaccount", "cradlepoint", "Cradlepoint",
                     "ethernet", "checkbox", "Checkbox", "deactivated", "uninstall", "uploads", "workflow", 
                     "admins", "Admins", "LinkedIn", "logon", "logout", "NetCIoud", "NetCloudManager", "Fedex", "FedEx", "fedex", "VPN", "vpn", "VPNs", "vpns", "CloudManager", "LANs", "subscriptons", "APN", "apn", "APNs", "firewalls",
                     "IPSEC", "IPSec", "ipsec", "IPsec", "Ipsec", "SSID", "failback", "icloud", "IPN", "NCOS", "OpenDNS", "openVPN", "OpenVPN", "SSID", "ssid", "SSId", "SSIDs", "ssids", "urls", "URLs", "username", "Username")

colnames(added_words) <- c("word")
added_words <- as.data.frame(added_words)

## This is to bind the dataframes

partner_connect_stopwords3 <-rbind(partner_connect_stopwords3, added_words)

## transform the dataframe list back to an unlisted vector

partner_connect_stopwords3 <- as.vector(unlist(partner_connect_stopwords3$word))

df <- removeWords(partner_connect_subset$SALESFORCE_CASE_DESCRIPTION, partner_connect_stopwords3)
df <- as.data.frame(df)
colnames(df) <- c("word")

## Splits concatenated data of a string into seperate values as a cell in the same column

split_data <- cSplit(df, "word", sep = " ", direction = "long")

## Transforms the data into unique

split_data <- unique(split_data)
split_data <- as.data.frame(split_data)

## This is the final version

stopwords3 <- as.vector(unlist(split_data$word))

## Save portal removed words for later

partner_removed_words <- as.data.frame(unlist(split_data$word))
colnames(partner_removed_words) <- c("partner_removed_words")


## This is to complete the Partner Connection subsection

uncleansed_partner_description_subset <- case_data_df %>% filter(SALESFORCE_CASE_ORIGIN == "Partner Connect")
partner_connect_subset$SALESFORCE_CASE_DESCRIPTION_Uncleansed <- uncleansed_partner_description_subset$SALESFORCE_CASE_DESCRIPTION
partner_connect_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(partner_connect_subset$SALESFORCE_CASE_DESCRIPTION, partner_connect_stopwords)
partner_connect_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(partner_connect_subset$SALESFORCE_CASE_DESCRIPTION, partner_connect_stopwords2)
partner_connect_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(partner_connect_subset$SALESFORCE_CASE_DESCRIPTION, partner_connect_stopwords3)
partner_connect_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(partner_connect_subset$SALESFORCE_CASE_DESCRIPTION, stopwords("english"))
partner_connect_subset$SALESFORCE_CASE_DESCRIPTION <- str_squish(partner_connect_subset$SALESFORCE_CASE_DESCRIPTION)


#########################################################################  SFDC Connect subsection ##############################################################################


## This is the SFDC Internal sub-section

## Start off the script with running the stop words specific for the SFDC Connect subsection


sfdc_Internal_stopwords = c("Cx", "cx", "calling in", "Hello", "hello", "cradlepoint", "Cradlepoint", "Device", "device", "case", "Case", "Please", "please", "Attached", "attach",
              "called", "Client", "client", "Case", "case", "Caller", "www", "CC", "cc", "com", "PM", "FA", "MAC", "Chat", "SN", "NCOS", "Chat started by",
              "Thank you for contacting", "To better assist you", "provide the following information", "Current NCOS Version", "http", "Device Information", "ab", "P", "Carrier",
              "K", "C", "AM", "J", "GED", "E", "A", "B", "C", "D", "E", "F", "G", "H", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
              "WA", "MM", "CBA", "NOCOS", "IBR", "Serial", "TB", "CM" ,"NNN", "IBRC", "CBALP", "AER", "DA", "WARA", "AE", "BA", "BB", "AC", "AT", "BODY",
              "NetCloud Engine","IBRC","AER","IBR","MANAGED SERVICES","CTR","MBR","Cradlepoint Connect","End of Support","Product Recommendation","W","R PLTE",
              "MCLPE VZ","MC","CBR","E","IBR FIPS","CBA","Accessories","L","CX","IBRB","CR","CBAB","AER FIPS","MBRB","MCLPE AT","Cradlepoint University","MBRv",
              "CradleCare","NetCloud Manager","AP","CR PoE","MCLPE GN","Beta","R","Cradlepoint Virtual Router", "VZW", "MCLP", "https", "Sent from my iPhone", "jpg")

## gender names that are in the kantrowitz dictionary

sfdc_Internal_stopwords2 <- c(kantrowitz$name)


sfdc_Internal_subset <- tibble(case_data_df)
sfdc_Internal_subset <- sfdc_Internal_subset %>% filter(SALESFORCE_CASE_ORIGIN == "SFDC Internal")
sfdc_Internal_subset$SALESFORCE_CASE_PRODUCT_C <- as.factor(sfdc_Internal_subset$SALESFORCE_CASE_PRODUCT_C)
sfdc_Internal_subset$SALESFORCE_CASE_DESCRIPTION <- sfdc_Internal_subset$SALESFORCE_CASE_DESCRIPTION %>% replace_na("Unknown")
sfdc_Internal_subset$SALESFORCE_CASE_DESCRIPTION <- str_replace_all(sfdc_Internal_subset$SALESFORCE_CASE_DESCRIPTION, "[^[:alnum:]]", " ")
sfdc_Internal_subset$SALESFORCE_CASE_DESCRIPTION <- gsub('[[:digit:]]+', '', sfdc_Internal_subset$SALESFORCE_CASE_DESCRIPTION)
## This is to get the words from the GradyAugmented dictionary and transform it as a dataframe

sfdc_Internal_stopwords3 <- as.data.frame(GradyAugmented)

## These are words you want to add to the dictionary, before they are removed

colnames(sfdc_Internal_stopwords3) <- c("word")
added_words <- rbind("ncm", "internet", "admin", "Admin", "amazonaws", "app", "apps", "att", "ATT", "authenticator", "Authenticator", "	AuthorizationError",
                     "blog", "Blog", "Boise", "CEO", "comcast", "config", "Config", "connectivity", "Connectivity", "controlware", "Controlware", "cradlepointcm",
                     "cradlepointecm", "CradlepointECM", "encryption", "Encryption", "endpoint", "Endpoint", "Firefox", "firewall", "firmware", "inbox", "internet", "Internet",
                     "jpg", "login", "Login", "logins", "Logins", "logoff", "malware", "ncm", "NCM", "netcloud", "Netcloud", "NetCloud", "NetCLoud", "NetClound", "Netcoud",
                     "Netcould", "onsite", "passcode", "Passthrough", "pw", "PW", "pwd", "screenshot", "servicedesk", "ServiceDesk", "servicessupport", "ServicesSupport",
                     "sku", "Sku","Skype","smartphone", "Smartphone", "sourcepage", "spam", "spams", "SPAM", "sysadmin", "Sysadmin", "wifi", "Wifi", "WIFI","youtube", "Youtube",
                     "YouTube", "analytics", "Analytics", "api", "API", "AutoAPN", "AWS", "broadband", "download", "Download", "downloads", "dropdown", "endpoints",
                     "html", "http", "https", "IOS", "iOS", "IoT", "iot", "IOT", "Iot", "IP", "ip", "iP", "Ip", "Modem", "MODEM", "modems", "Modems", "nectcloud",
                     "Speedtest", "speedtest", "speedtests", "spreadsheet", "spreadsheets", "Tmobile", "tmobile", "TMobile","unregister", "Unregister", "unregistered",
                     "unregistering", "upload", "Upload", "URL", "url", "Verizon", "verizon", "zscaler", "Zscaler", "Ericsson", "email", "Email", "failover", "Failover",
                     "offline", "Offline", "OFFLINE", "Okta", "OKTA","online", "Online","upload", "Upload", "uploaded", "autovpn", "AutoVPN", "autoVPN", "failover", "failovers",
                     "iOs", "IoT", "IOT", "LAN", "lan", "laptop", "Laptop", "laptops", "linux", "Linux", "netgear", "Netgear", "offline", "Offline", "Online", "Online",
                     "Okta", "okta", "reboot", "rebooting", "Rebooted", "Rebooting", "speedtest", "Speedtest", "subaccount", "Subaccount", "cradlepoint", "Cradlepoint",
                     "ethernet", "checkbox", "Checkbox", "deactivated", "uninstall", "uploads", "workflow",
                     "admins", "Admins", "LinkedIn", "logon", "logout", "NetCIoud", "NetCloudManager", "Fedex", "FedEx", "fedex", "VPN", "vpn", "VPNs", "vpns", "CloudManager", "LANs", "subscriptons", "APN", "apn", "APNs", "firewalls",
                     "IPSEC", "IPSec", "ipsec", "IPsec", "Ipsec", "SSID", "failback", "icloud", "IPN", "NCOS", "OpenDNS", "openVPN", "OpenVPN", "SSID", "ssid", "SSId", "SSIDs", "ssids", "urls", "URLs", "username", "Username")

colnames(added_words) <- c("word")
added_words <- as.data.frame(added_words)

## This is to bind the dataframes

sfdc_Internal_stopwords3 <-rbind(sfdc_Internal_stopwords3, added_words)

## transform the dataframe list back to an unlisted vector

sfdc_Internal_stopwords3 <- as.vector(unlist(sfdc_Internal_stopwords3$word))

df <- removeWords(sfdc_Internal_subset$SALESFORCE_CASE_DESCRIPTION, sfdc_Internal_stopwords3)
df <- as.data.frame(df)
colnames(df) <- c("word")

## Splits concatenated data of a string into seperate values as a cell in the same column

split_data <- cSplit(df, "word", sep = " ", direction = "long")

## Transforms the data into unique

split_data <- unique(split_data)
split_data <- as.data.frame(split_data)

## This is the final version

sfdc_Internal_stopwords3 <- as.vector(unlist(split_data$word))

## Save portal removed words for later

sfdc_removed_words <- as.data.frame(unlist(split_data$word))
colnames(sfdc_removed_words) <- c("sfdc_removed_words")

## This is to complete the SFDC Internal subsection

uncleansed_sfdc_Internal_description_subset <- case_data_df %>% filter(SALESFORCE_CASE_ORIGIN == "SFDC Internal")
sfdc_Internal_subset$SALESFORCE_CASE_DESCRIPTION_Uncleansed <- uncleansed_sfdc_Internal_description_subset$SALESFORCE_CASE_DESCRIPTION
sfdc_Internal_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(sfdc_Internal_subset$SALESFORCE_CASE_DESCRIPTION, sfdc_Internal_stopwords)
sfdc_Internal_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(sfdc_Internal_subset$SALESFORCE_CASE_DESCRIPTION, sfdc_Internal_stopwords2)
sfdc_Internal_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(sfdc_Internal_subset$SALESFORCE_CASE_DESCRIPTION, sfdc_Internal_stopwords3)
sfdc_Internal_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(sfdc_Internal_subset$SALESFORCE_CASE_DESCRIPTION, stopwords("english"))
sfdc_Internal_subset$SALESFORCE_CASE_DESCRIPTION <- str_squish(sfdc_Internal_subset$SALESFORCE_CASE_DESCRIPTION)

#########################################################################  JIRA Connect subsection ##############################################################################

## This is the JIRA sub-section

## Start off the script with running the stop words specific for the JIRA Connect subsection


jira_stopwords = c("Cx", "cx", "calling in", "Hello", "hello", "cradlepoint", "Cradlepoint", "Device", "device", "case", "Case", "Please", "please", "Attached", "attach",
              "called", "Client", "client", "Case", "case", "Caller", "www", "CC", "cc", "com", "PM", "FA", "MAC", "Chat", "SN", "NCOS", "Chat started by",
              "Thank you for contacting", "To better assist you", "provide the following information", "Current NCOS Version", "http", "Device Information", "ab", "P", "Carrier",
              "K", "C", "AM", "J", "GED", "E", "A", "B", "C", "D", "E", "F", "G", "H", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
              "WA", "MM", "CBA", "NOCOS", "IBR", "Serial", "TB", "CM" ,"NNN", "IBRC", "CBALP", "AER", "DA", "WARA", "AE", "BA", "BB", "AC", "AT", "BODY",
              "NetCloud Engine","IBRC","AER","IBR","MANAGED SERVICES","CTR","MBR","Cradlepoint Connect","End of Support","Product Recommendation","W","R PLTE",
              "MCLPE VZ","MC","CBR","E","IBR FIPS","CBA","Accessories","L","CX","IBRB","CR","CBAB","AER FIPS","MBRB","MCLPE AT","Cradlepoint University","MBRv",
              "CradleCare","NetCloud Manager","AP","CR PoE","MCLPE GN","Beta","R","Cradlepoint Virtual Router", "VZW", "MCLP", "https", "Sent from my iPhone", "jpg")

## gender names that are in the kantrowitz dictionary

jira_stopwords2 <- c(kantrowitz$name)

jira_subset <- tibble(case_data_df)
jira_subset <- jira_subset %>% filter(SALESFORCE_CASE_ORIGIN == "JIRA")
jira_subset$SALESFORCE_CASE_PRODUCT_C <- as.factor(jira_subset$SALESFORCE_CASE_PRODUCT_C)
jira_subset$SALESFORCE_CASE_DESCRIPTION <- jira_subset$SALESFORCE_CASE_DESCRIPTION %>% replace_na("Unknown")
jira_subset$SALESFORCE_CASE_DESCRIPTION <- str_replace_all(jira_subset$SALESFORCE_CASE_DESCRIPTION, "[^[:alnum:]]", " ")
jira_subset$SALESFORCE_CASE_DESCRIPTION <- gsub('[[:digit:]]+', '', jira_subset$SALESFORCE_CASE_DESCRIPTION)

## This is to get the words from the GradyAugmented dictionary and transform it as a dataframe

jira_stopwords3 <- as.data.frame(GradyAugmented)

## These are words you want to add to the dictionary, before they are removed

colnames(jira_stopwords3) <- c("word")
added_words <- rbind("ncm", "internet", "admin", "Admin", "amazonaws", "app", "apps", "att", "ATT", "authenticator", "Authenticator", "	AuthorizationError",
                     "blog", "Blog", "Boise", "CEO", "comcast", "config", "Config", "connectivity", "Connectivity", "controlware", "Controlware", "cradlepointcm",
                     "cradlepointecm", "CradlepointECM", "encryption", "Encryption", "endpoint", "Endpoint", "Firefox", "firewall", "firmware", "inbox", "internet", "Internet",
                     "jpg", "login", "Login", "logins", "Logins", "logoff", "malware", "ncm", "NCM", "netcloud", "Netcloud", "NetCloud", "NetCLoud", "NetClound", "Netcoud",
                     "Netcould", "onsite", "passcode", "Passthrough", "pw", "PW", "pwd", "screenshot", "servicedesk", "ServiceDesk", "servicessupport", "ServicesSupport",
                     "sku", "Sku","Skype","smartphone", "Smartphone", "sourcepage", "spam", "spams", "SPAM", "sysadmin", "Sysadmin", "wifi", "Wifi", "WIFI","youtube", "Youtube",
                     "YouTube", "analytics", "Analytics", "api", "API", "AutoAPN", "AWS", "broadband", "download", "Download", "downloads", "dropdown", "endpoints",
                     "html", "http", "https", "IOS", "iOS", "IoT", "iot", "IOT", "Iot", "IP", "ip", "iP", "Ip", "Modem", "MODEM", "modems", "Modems", "nectcloud",
                     "Speedtest", "speedtest", "speedtests", "spreadsheet", "spreadsheets", "Tmobile", "tmobile", "TMobile","unregister", "Unregister", "unregistered",
                     "unregistering", "upload", "Upload", "URL", "url", "Verizon", "verizon", "zscaler", "Zscaler", "Ericsson", "email", "Email", "failover", "Failover",
                     "offline", "Offline", "OFFLINE", "Okta", "OKTA","online", "Online","upload", "Upload", "uploaded", "autovpn", "AutoVPN", "autoVPN", "failover", "failovers",
                     "iOs", "IoT", "IOT", "LAN", "lan", "laptop", "Laptop", "laptops", "linux", "Linux", "netgear", "Netgear", "offline", "Offline", "Online", "Online",
                     "Okta", "okta", "reboot", "rebooting", "Rebooted", "Rebooting", "speedtest", "Speedtest", "subaccount", "Subaccount", "cradlepoint", "Cradlepoint",
                     "ethernet", "checkbox", "Checkbox", "deactivated", "uninstall", "uploads", "workflow",
                     "admins", "Admins", "LinkedIn", "logon", "logout", "NetCIoud", "NetCloudManager", "Fedex", "FedEx", "fedex", "VPN", "vpn", "VPNs", "vpns", "CloudManager", "LANs", "subscriptons", "APN", "apn", "APNs", "firewalls",
                     "IPSEC", "IPSec", "ipsec", "IPsec", "Ipsec", "SSID", "failback", "icloud", "IPN", "NCOS", "OpenDNS", "openVPN", "OpenVPN", "SSID", "ssid", "SSId", "SSIDs", "ssids", "urls", "URLs", "username", "Username")
colnames(added_words) <- c("word")
added_words <- as.data.frame(added_words)

## This is to bind the dataframes

jira_stopwords3 <-rbind(jira_stopwords3, added_words)
## transform the dataframe list back to an unlisted vector
jira_stopwords3 <- as.vector(unlist(jira_stopwords3$word))

df <- removeWords(jira_subset$SALESFORCE_CASE_DESCRIPTION, jira_stopwords3)
df <- as.data.frame(df)
colnames(df) <- c("word")

## Splits concatenated data of a string into seperate values as a cell in the same column

split_data <- cSplit(df, "word", sep = " ", direction = "long")

## Transforms the data into unique

split_data <- unique(split_data)
split_data <- as.data.frame(split_data)

## This is the final version

jira_stopwords3 <- as.vector(unlist(split_data$word))

## Save portal removed words for later

jira_removed_words <- as.data.frame(unlist(split_data$word))
colnames(jira_removed_words) <- c("jira_removed_words")

## This is to complete the JIRA Connect subsection

uncleansed_jira_description_subset <- case_data_df %>% filter(SALESFORCE_CASE_ORIGIN == "JIRA")
jira_subset$SALESFORCE_CASE_DESCRIPTION_Uncleansed <- uncleansed_jira_description_subset $SALESFORCE_CASE_DESCRIPTION
jira_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(jira_subset$SALESFORCE_CASE_DESCRIPTION, jira_stopwords)
jira_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(jira_subset$SALESFORCE_CASE_DESCRIPTION, jira_stopwords2)
jira_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(jira_subset$SALESFORCE_CASE_DESCRIPTION, jira_stopwords3)
jira_subset$SALESFORCE_CASE_DESCRIPTION <- removeWords(jira_subset$SALESFORCE_CASE_DESCRIPTION, stopwords("english"))
jira_subset$SALESFORCE_CASE_DESCRIPTION <- str_squish(jira_subset$SALESFORCE_CASE_DESCRIPTION)


############################################################################  Combine Dataset ######################################################################################

# Create final dataset for export for main data

case_subset <- bind_rows(phone_subset, chat_subset, portal_subset, email_subset, saas_subset, partner_connect_subset, sfdc_Internal_subset, jira_subset, forum_subset)


write.csv(case_subset, file = "CASE_DATA_CLEANSED.csv", row.names = FALSE)


# Create final dataset for export for missing words

removed_english_words <- stopwords("english")
removed_english_words <- as.data.frame(removed_english_Words)
removed_common_words <- stopwords
removed_common_words <- as.data.frame(removed_common_Words)
removed_words_dataset <- rbind.fill(saas_removed_words, chat_removed_words, portal_removed_words, email_removed_words, phone_removed_words, partner_removed_words, sfdc_removed_words, jira_removed_words, forum_removed_words, removed_common_words, removed_english_words)
removed_words_dataset[is.na(removed_words_dataset)] <- " "


write.csv(removed_words_dataset, file = "CASE_DATA_REMOVED_WORDS.csv", row.names = FALSE)


## if need be, change the date

case_subset$SALESFORCE_CASE_CREATED_DATE <- as.Date(case_subset$SALESFORCE_CASE_CREATED_DATE)
case_subset$SALESFORCE_CASE_CLOSED_DATE <- as.Date(case_subset$SALESFORCE_CASE_CLOSED_DATE)

## Connect to the particular Warehouse, Database, and Schema for this project

warehouse <- DBI::dbGetQuery(myconn,"USE WAREHOUSE IS_DEV_XS")
database <- DBI::dbGetQuery(myconn, "USE DATABASE PROD_MGMT_DB")
schema <- DBI::dbGetQuery(myconn, "USE SCHEMA CASE_DATA")

## Count the number of characters for each column

nchar(case_subset$SALESFORCE_CASE_DESCRIPTION)

## Sum of the number of characters for each column

sum(nchar(case_subset$SALESFORCE_CASE_DESCRIPTION))


## Product a SQL Table, based on dataframe


DBI::sqlCreateTable(myconn, "CASE_DATA_R", case_subset)


## Check to see if table already exist


dbExistsTable(myconn, "CASE_DATA_R")


## Create a table in Snowflake, based on selected criteria

dbExecute(
myconn,
'CREATE OR REPLACE TABLE "CASE_DATA_R" (
"SALESFORCE_CASE_CREATED_DATE" DATE,
"SALESFORCE_CASE_CLOSED_DATE" DATE,
"SALESFORCE_CASE_ID" VARCHAR(60000),
"SALESFORCE_CASE_ACCOUNT_ID" VARCHAR(60000),
"SALESFORCE_CASE_CASE_NUMBER" VARCHAR(60000),
"SALESFORCE_CASE_STATUS" VARCHAR(60000),
"SALESFORCE_CASE_ORIGIN" VARCHAR(60000),
"SALESFORCE_CASE_SUBJECT" VARCHAR(60000),
"SALESFORCE_CASE_PRIORITY" VARCHAR(60000),
"SALESFORCE_CASE_DESCRIPTION" VARCHAR(60000),
"SALESFORCE_CASE_PRODUCT_C" VARCHAR(60000),
"SALESFORCE_CASE_FIRMWARE_VERSION_C" VARCHAR(60000),
"SALESFORCE_CASE_MODEM_TYPE_C" VARCHAR(60000),
"SALESFORCE_CASE_CARRIER_C" VARCHAR(60000),
"SALESFORCE_CASE_CASE_TYPE_C" VARCHAR(60000),
"SALESFORCE_CASE_REASON_CODES_C" VARCHAR(60000),
"SALESFORCE_CASE_REASON_CODE_DETAIL_C" VARCHAR(60000),
"SALESFORCE_CASE_TRIAGE_LEVEL_1" VARCHAR(60000),
"SALESFORCE_CASE_TRIAGE_LEVEL_2" VARCHAR(60000),
"SALESFORCE_CASE_TRIAGE_LEVEL_3" VARCHAR(60000),
"SALESFORCE_CASE_ESCALATION_C" VARCHAR(60000)
)'
)

## Read the data table that was just created


Sql_table <- dbReadTable(myconn, "CASE_DATA_R")
Sql_table

## Write table into snowflake

DBI::dbWriteTable(myconn, "CASE_DATA_R_R", value = removed_words_dataset, row.names = FALSE, overwrite = TRUE, append = FALSE)


## Write table based on field type properties into snowflake

case_subset <- bind_rows(phone_subset, chat_subset, portal_subset, email_subset, saas_subset, partner_connect_subset, sfdc_Internal_subset, jira_subset, forum_subset)

case_subset$SALESFORCE_CASE_PRODUCT_C <- as.character(case_subset$SALESFORCE_CASE_PRODUCT_C)

DBI::dbWriteTable(myconn, "CASE_DATA_R", value = case_subset, row.names = FALSE, overwrite = TRUE, append = FALSE,
                  field.types = c(
                    SALESFORCE_CASE_CREATED_DATE  = "DATETIME",
                    SALESFORCE_CASE_CLOSED_DATE = "DATETIME",
                    SALESFORCE_CASE_ID = "VARCHAR(16777216)",
                    SALESFORCE_CASE_ACCOUNT_ID = "VARCHAR(16777216)",
                    SALESFORCE_CASE_CASE_NUMBER = "VARCHAR(16777216)",
                    SALESFORCE_CASE_STATUS = "VARCHAR(16777216)",
                    SALESFORCE_CASE_ORIGIN = "VARCHAR(16777216)",
                    SALESFORCE_CASE_SUBJECT = "VARCHAR(16777216)",
                    SALESFORCE_CASE_PRIORITY = "VARCHAR(16777216)",
                    # SALESFORCE_CASE_DESCRIPTION  = "VARCHAR(16777216)",
                    SALESFORCE_CASE_PRODUCT_C = "VARCHAR(16777216)",
                    SALESFORCE_CASE_FIRMWARE_VERSION_C = "VARCHAR(16777216)",
                    SALESFORCE_CASE_MODEM_TYPE_C = "VARCHAR(16777216)",
                    SALESFORCE_CASE_CARRIER_C = "VARCHAR(16777216)",
                    SALESFORCE_CASE_CASE_TYPE_C = "VARCHAR(16777216)",
                    SALESFORCE_CASE_REASON_CODES_C = "VARCHAR(16777216)",
                    SALESFORCE_CASE_REASON_CODE_DETAIL_C = "VARCHAR(16777216)",
                    SALESFORCE_CASE_TRIAGE_LEVEL_1 = "VARCHAR(16777216)",
                    SALESFORCE_CASE_TRIAGE_LEVEL_2 = "VARCHAR(16777216)",
                    SALESFORCE_CASE_TRIAGE_LEVEL_3 = "VARCHAR(16777216)",
                    SALESFORCE_CASE_ESCALATION_C = "VARCHAR(16777216)"))


## Check table again


Sql_table <- dbReadTable(myconn, "CASE_DATA_R")
Sql_table

## This is to remove a table of choice

dbRemoveTable(myconn, "CASE_DATA_R_R")

## If need be query directly from the table


dbGetQuery(myconn, "SELECT SALESFORCE_CASE_CREATED_DATE 
                    From CASE_DATA_R")


## Write into csv file if necessary

write.csv(case_subset, file = "CASE_DATA_CLEANSED.csv", row.names = FALSE)



############################################################################ Miscellaneous ######################################################################################

# This is for a blue word cloud


library(DT)
library(tidytext)
library(dplyr)
library(stringr)
library(sentimentr)
library(ggplot2)
library(RColorBrewer)
library(readr)
library(SnowballC)
library(tm)
library(wordcloud)
library(reticulate)
library(crfsuite)


words <- case_subset %>%
  select(c("SALESFORCE_CASE_ID", "SALESFORCE_CASE_DESCRIPTION", "SALESFORCE_CASE_ORIGIN")) %>%
  unnest_tokens(word, SALESFORCE_CASE_DESCRIPTION) %>%
  filter(!word %in% stop_words$word)

words


afinn <- get_sentiments("afinn") %>% mutate(word = wordStem(word))
reviews.afinn <- words %>%
  inner_join(afinn, by = "word")
head(reviews.afinn)



word_summary <- reviews.afinn %>%
  group_by(word) %>%
  summarise(mean_rating = mean(value), score = max(value), count_word = n()) %>%
  arrange(desc(count_word))


word_summary 



review_summary <- reviews.afinn %>%
  group_by(SALESFORCE_CASE_ORIGIN) %>%
  summarise(mean_rating = mean(overall), sentiment = mean(score))
datatable(head(review_summary))

reviews.afinn


ggplot(filter(word_summary, count_word < 50000), aes(mean_rating, score)) + 
  geom_text(aes(label = word, color = count_word, size=count_word), position= position_jitter()) + 
  scale_color_gradient(low = "lightblue", high = "darkblue") 
+ coord_cartesian(xlim=c(3.5,4.5)) + 
  guides(size = FALSE, color=FALSE)

