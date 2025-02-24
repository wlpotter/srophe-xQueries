xquery version "3.0";
(: 
 : Add/replace new bibl elements with ptr elements to newly created bibl TEI records 
 : Save changes to db, must be logged into eXist as admin/with admin privileges
 : :)

declare default element namespace "http://www.tei-c.org/ns/1.0";
declare namespace tei = "http://www.tei-c.org/ns/1.0";
declare namespace syriaca = "http://syriaca.org";
declare namespace functx = "http://www.functx.com";

(: Find matching biblStruct. Match bibl id against @corresp list. Match ids with optional 'a' or 'b' at the end as well :)
declare function syriaca:create-new-bibl($id, $node) {
    for $bibl in $node//tei:biblStruct[matches(@corresp, concat('(^|\W)',$id,'[a-z]?(\W|$)'))]
    let $bibl-id := analyze-string($bibl/@corresp, concat('(^|\W)',$id,'[a-z]?(\W|$)'))/fn:match/text()
    let $new-bibl := 
            <bibl xmlns="http://www.tei-c.org/ns/1.0" xml:id="{$bibl-id}">
                {(  
                    $bibl/descendant::tei:title,
                    <ptr target="{$bibl//tei:idno[contains(.,'syriaca')]/text()}"/>,
                    functx:remove-attributes($bibl/tei:citedRange[@corresp = $bibl-id],'corresp')
                )}
            </bibl>
    return $new-bibl
};

declare function functx:distinct-deep
  ( $nodes as node()* )  as node()* {

    for $seq in (1 to count($nodes))
    return $nodes[$seq][not(functx:is-node-in-sequence-deep-equal(
                          .,$nodes[position() < $seq]))]
 } ;
 
 declare function functx:is-node-in-sequence-deep-equal
  ( $node as node()? ,
    $seq as node()* )  as xs:boolean {

   some $nodeInSeq in $seq satisfies deep-equal($nodeInSeq,$node)
 } ;

declare function functx:remove-attributes
  ( $elements as element()* ,
    $names as xs:string* )  as element()* {

   for $element in $elements
   return element
     {node-name($element)}
     {$element/@*[not(functx:name-test(name(),$names))],
      $element/node() }
 } ;
 
 declare function functx:name-test
  ( $testname as xs:string? ,
    $names as xs:string* )  as xs:boolean {

$testname = $names
or
$names = '*'
or
functx:substring-after-if-contains($testname,':') =
   (for $name in $names
   return substring-after($name,'*:'))
or
substring-before($testname,':') =
   (for $name in $names[contains(.,':*')]
   return substring-before($name,':*'))
 } ;
 
 declare function functx:substring-after-if-contains
  ( $arg as xs:string? ,
    $delim as xs:string )  as xs:string? {

   if (contains($arg,$delim))
   then substring-after($arg,$delim)
   else $arg
 } ;

(: Copy of latest xml data to be converted. This is my local path :)
let $uri := "/db/apps/srophe-data/data/works/title-list-for-translit.xml"
let $new-doc := doc($uri)/TEI
(: Run on all works :)
let $works := collection('/db/apps/srophe-data/data/works/tei')//body/bibl
let $titles := 
    for $work in $works
    let $syriac-title := $work/title[@xml:lang='syr' and contains(@syriaca-tags,'#syriaca-headword')]
    return 
        if ($syriac-title) then 
            <bibl>{($work/idno[contains(.,'syriaca')],$syriac-title)}</bibl>
            else ()
return update insert $titles into $new-doc