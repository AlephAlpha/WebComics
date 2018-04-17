#!/usr/bin/env wolframscript

SetDirectory[
  FromCharacterCode[
   ToCharacterCode@"/home/alephalpha/文档/马甲与小号/WebComics/", "UTF8"]];

Get["access_token.mx"];

appURL = " http://example.com";

URLExecute[
  HTTPRequest[
   "https://api.weibo.com/2/statuses/share.json", <|Method -> "POST", 
    "Body" -> {"access_token" -> accessToken, 
      "status" -> 
       FromCharacterCode[ToCharacterCode@$ScriptCommandLine[[2]], 
         "UTF8"] <> appURL}|>]] // Print
