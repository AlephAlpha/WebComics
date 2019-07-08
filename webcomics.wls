#!/usr/bin/env wolframscript

appURL = " https://github.com/AlephAlpha/WebComics";
weiboAPI = "https://api.weibo.com/2/statuses/share.json";

(* 请设成脚本所在的目录 *)
(* 目录名有中文时必须先转 UTF8 *)
SetDirectory[
  FromCharacterCode[
   ToCharacterCode@"~/文档/Wolfram Mathematica/WebComics/", "UTF8"]];

(* 把下载到的图片放在一个临时文件夹 *)
tmpDir = CreateDirectory[];

(* 从另一个文件中读取 accessToken *)
(* 第一次使用前请用 DumpSave 函数把 accessToken 保存到这个文件 *)
Get["access_token.mx"];

(* 请根据所使用的翻墙软件修改此处的参数 *)
(* 只在读取 RSS 时会用到，下载图片时不用翻墙 *)
SetInternetProxy["HTTP", {"127.0.0.1", 1080}];
ProxyImport[url_, options___] := Module[{data},
   UseInternetProxy[True];
   data = Import[url, options];
   UseInternetProxy[False];
   data];

(* 上传图片，发布微博，并记录原漫画的发布时间 *)
(* 不知为何 Mathematica 自带的关于网络的函数全部用不了，只能调用 curl *)
Post[name_][pubDate_, title_, link_, image_String] := 
  Import["!curl -F 'access_token=" <> accessToken <> "' -F 'status=" <>
     URLEncode["[" <> name <> "]" <> link <> " " <> title <> appURL] <>
     "' -F 'pic=@" <> 
    ExpandFileName[URLDownload[image, tmpDir]] <> "' " <> weiboAPI, "JSON"];

Post[name_][pubDate_, title_, link_, image_Integer: 1] :=
  Post[name][pubDate, title, link, 
   Import[link, "ImageLinks"][[image]]];

(* 根据 pattern 来从 RSS 中获取信息，并用 Post 函数发布微博 *)
ShareWebComics[name_, rss_, pattern_] :=
  Module[{data},
   data = Sort[
     Select[Cases[ProxyImport[rss, "XML"], pattern, {0, Infinity}],
      First@# > Lookup[pubDates, name, 0] &]];
   Post[name] @@@ data;
   pubDates[name] = Max[Lookup[pubDates, name, 0], First /@ data];];

(* 从另一个文件中读取已转发的漫画的发布时间 *)
(* 第一次使用前请用 DumpSave 函数把 pubDates 保存到这个文件 *)
Get["webcomics.mx"];

(* 用 ShareWebComics 函数转发各网漫 *)

ShareWebComics["xkcd", "http://xkcd.com/rss.xml",
  XMLElement["item", {},
    {XMLElement["title", {}, {title_}],
     XMLElement["link", {}, {link_}],
     XMLElement["description", {}, {description_}],
     XMLElement["pubDate", {}, {pubDate_}], ___}] :>
   {AbsoluteTime@StringDrop[pubDate, -5], title, link,
    First@
     StringCases[description, 
      "<img src=\"" ~~ Shortest[src__] ~~ "\"" ~~ ___ :> src]}];

ShareWebComics["Buni", "http://www.bunicomic.com/feed/",
  XMLElement["item", {},
    {XMLElement["title", {}, {"Buni"}],
     XMLElement["link", {}, {link_}], ___,
     XMLElement["pubDate", {}, {pubDate_}], ___,
     XMLElement["description", {}, {description_}], ___}] :>
   {AbsoluteTime@StringDrop[pubDate, -5], "", link,
    First@StringCases[description,
      "src=\"" ~~ Shortest[src__] ~~ "-150x150" ~~ Shortest[fmt__] ~~ 
        "\"" ~~ ___ :>
       src <> fmt]}];

ShareWebComics["Optipess", "http://feeds.feedburner.com/Optipess?format=xml",
  XMLElement["item", {},
    {XMLElement["title", {}, {title_}], ___,
     XMLElement["pubDate", {}, {pubDate_}], ___,
     XMLElement["category", {}, {"Comic"}], ___,
     XMLElement["description", {}, {description_}], ___,
     XMLElement[{___, "origLink"}, {}, {link_}], ___}] :>
   {AbsoluteTime@StringDrop[pubDate, -5], title, link,
    First@StringCases[description,
      "<img src=\"" ~~ Shortest[src__] ~~ "\"" ~~ ___ :> src]}];

ShareWebComics["Buttersafe", "http://feeds.feedburner.com/Buttersafe?format=xml",
  XMLElement["item", {},
    {XMLElement["title", {}, {title_}], ___,
     XMLElement["pubDate", {}, {pubDate_}], ___,
     XMLElement["category", {}, {"Comic"}], ___,
     XMLElement["description", {}, {description_}], ___,
     XMLElement[{___, "origLink"}, {}, {link_}], ___}] :>
   {AbsoluteTime@StringDrop[pubDate, -5], title, link,
    First@StringCases[description,
      "<img src=\"" ~~ Shortest[src1__] ~~ "/rss" ~~ 
        Shortest[src2__] ~~ "RSS" ~~ Shortest[src3__] ~~ "\"" ~~ ___ :>
       src1 <> src2 <> src3]}];

ShareWebComics["SMBC", "http://www.smbc-comics.com/rss.php",
  XMLElement["item", {},
    {XMLElement["title", {}, {title_}],
     XMLElement["description", {}, {description_}],
     XMLElement["link", {}, {link_}], ___,
     XMLElement["pubDate", {}, {pubDate_}], ___}] :>
   {AbsoluteTime@StringDrop[pubDate, -5], StringDrop[title, 36], 
    link,
    First@StringCases[description,
      "<img src=\"" ~~ Shortest[src__] ~~ "\"" ~~ ___ :> src]}];

ShareWebComics["SpikedMath", "http://feeds.feedburner.com/SpikedMath?format=xml",
  XMLElement["entry", {},
    {XMLElement["title", {}, {title_}], ___,
     XMLElement["published", {}, {pubDate_}], ___,
     XMLElement["content", {___}, {description_}], ___,
     XMLElement[{___, "origLink"}, {}, {link_}], ___}] :>
   {AbsoluteTime@pubDate, title, link,
    First@StringCases[description,
      "<img src=\"" ~~ Shortest[src__] ~~ "\"" ~~ ___ :> src]}];

ShareWebComics["ExtraOrdinary", "http://www.exocomics.com/feed",
  XMLElement["item", {},
    {XMLElement["title", {}, {title_}],
     XMLElement["link", {}, {link_}], ___,
     XMLElement["pubDate", {}, {pubDate_}], ___,
     XMLElement[{_, "encoded"}, {}, {description_}], ___}] :>
   {AbsoluteTime@StringDrop[pubDate, -5], title, link,
    First@StringCases[description,
      "src=\"" ~~ Shortest[src1__] ~~ "/feed" ~~ Shortest[src2__] ~~ 
        "\"" ~~ ___ :>
       src1 <> "/comics" <> src2]}];

ShareWebComics["pbfcomics", "http://pbfcomics.com/feed/",
  XMLElement["item", {},
    {XMLElement["title", {}, {title_}], ___,
     XMLElement["link", {}, {link_}], ___,
     XMLElement["pubDate", {}, {pubDate_}], ___}] :>
   {AbsoluteTime@StringDrop[pubDate, -5], title, link, 11}];

ShareWebComics["Geek&Poke", "http://feeds.feedburner.com/GeekAndPoke",
  XMLElement["item", {},
    {XMLElement["title", {}, {title_}], ___,
     XMLElement["pubDate", {}, {pubDate_}], ___,
     XMLElement["description", {}, {description_}], ___,
     XMLElement[{___, "origLink"}, {}, {link_}], ___}] :>
   {AbsoluteTime@StringDrop[pubDate, -5], title, link,
    First@StringCases[description,
      " data-image=\"" ~~ Shortest[src__] ~~ "\"" ~~ ___ :> src]}];

ShareWebComics["AbstruseGoose", "http://abstrusegoose.com/feed.xml", 
  XMLElement["item", {},
    {XMLElement["title", {}, {title_}],
     XMLElement["link", {}, {link_}], ___,
     XMLElement["pubDate", {}, {pubDate_}],
     XMLElement["description", {}, {description_}], ___}] :>
   {AbsoluteTime@pubDate, title, link,
    First@
     StringCases[description, 
      "<img src=\"" ~~ Shortest[src__] ~~ "\"" ~~ ___ :> src]}];

ShareWebComics["FalseKnees", "https://www.comicsrss.com/rss/false-knees.rss",
  XMLElement["item", {},
    {XMLElement["title", {}, {title_}], ___,
     XMLElement["pubDate", {}, {pubDate_}],
     XMLElement["description", {}, {description_}], ___}] :>
   {AbsoluteTime@pubDate, title, "http://falseknees.com",
    First@
     StringCases[description, 
      "<img src=\"" ~~ Shortest[src__] ~~ "\"" ~~ ___ :> src]}];

(* 保存漫画的发布时间 *)
DumpSave["webcomics.mx", pubDates];
