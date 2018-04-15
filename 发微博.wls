#!/usr/bin/env wolframscript

Get["access_token.mx"];

URLExecute[
  HTTPRequest[
   "https://api.weibo.com/2/statuses/share.json", <|Method -> "POST", 
    "Body" -> {"access_token" -> accessToken, 
      "status" -> 
       FromCharacterCode[ToCharacterCode@$ScriptCommandLine[[2]], 
         "UTF8"] <> " http://example.com/"}|>]] // Print
