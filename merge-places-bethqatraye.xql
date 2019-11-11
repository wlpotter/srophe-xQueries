xquery version "3.0";

(: SCRIPT FOR MERGING BETH QATRAYE PLACES RECORDS. BASED UPON THE :)
(: SCRIPT FOR MERGING PERSON RECORDS by ngibson :)
 
(:  INSTRUCTIONS: :)
(:  - Run this script on your local eXist-db installation. :)
(:  - You will need to scroll down to the VARIABLES section:)
(:  and change the $master-uri and $secondary-uri to the URIs of the records you want to merge. Also change the username. :)
(:  - Most of the information will be kept from both the master record and the secondary record, :)
(:  but for items that there should be only one of, the version from the master record will be kept :)
(:  (e.g., the Syriaca idnos, the #syriaca-headword tag, etc.). When there is matching data in the two records but the :)
(:  sources are different, the script *should* combine the two nodes but add another value to the @source. :)
(:  - The script is far from perfect. Try running it and then viewing the differences between the input and output files, :)
(:  using Oxygen's file diff tool or Github. :)
  
(:  KNOWN LIMITATIONS :)
(:  - No @syriaca-tags are preserved from the secondary record.:)
(:  - Source attributes pointing to bibls that don't have a ptr/@target will not be merged correctly. :)
(:    (The script uses the ptr/@target for matching with the correct source bibl.):)
(:  - <editor> in titleStmt may sometimes be duplicated:)
(:  - Updating links (syriaca:update-person-work-links) may only work for 1 record at a time:)
 

(: NAMESPACES:)
declare default element namespace "http://www.tei-c.org/ns/1.0";
declare namespace tei = "http://www.tei-c.org/ns/1.0";
declare namespace syriaca = "http://syriaca.org";
declare namespace functx = "http://www.functx.com";


(: CUSTOM FUNCTIONS :)

(: COMPARISON FUNCTIONS :)
declare function functx:is-node-in-sequence-deep-equal
  ( $node as node()? ,
    $seq as node()* )  as xs:boolean {

   some $nodeInSeq in $seq satisfies deep-equal($nodeInSeq,$node)
 } ;
declare function functx:distinct-deep
  ( $nodes as node()* )  as node()* {

    for $seq in (1 to count($nodes))
    return $nodes[$seq][not(functx:is-node-in-sequence-deep-equal(
                          .,$nodes[position() < $seq]))]
 } ;

(: ATTRIBUTE UPDATING FUNCTIONS :)
declare function syriaca:next-id($ids as xs:string*,$prefix as xs:string*,$i as xs:double*)
as xs:string*
{
    let $offset := if ($i) then $i else 1
    return 
        if (count($ids) > 0) then
            let $id-nums := 
                for $id in $ids[matches(.,'\d$')]
                return number(replace($id,$prefix,''))
            return concat($prefix,($i+max($id-nums)))
        else concat($prefix,$i)
};

declare function syriaca:remove-extra-attributes($input-node as node()*,$attributes-to-remove as xs:string*)
as node()*
{
    for $node in $input-node
    return
        if ($node/descendant-or-self::*[@*/name()=$attributes-to-remove]) then
            element {xs:QName(name($node))} {
                $node/@*[not(name()=$attributes-to-remove)], 
                syriaca:remove-extra-attributes($node/node(), $attributes-to-remove)}
        else $node
};

declare function syriaca:remove-attribute-values($input-node as node()*,$attributes as xs:string*,$attribute-values as xs:string*)
as node()*
{
    for $node in $input-node
    return
        if ($node/descendant-or-self::*[@*/name()=$attributes]) then
            let $updated-attributes := 
                for $value in $node/@*[name()=$attributes]
                    for $attribute at $i in $attributes
                    return 
                        if ($node/@*[name()=$attribute]) then 
                            attribute {$attribute} {replace($value,concat('[\s]*',$attribute-values[$i],'[\s]*'),'')}
                        else()
            return element {xs:QName(name($node))} {
                ($node/@*[not(name()=$attributes)], 
                $updated-attributes[.!='']),
                syriaca:remove-attribute-values($node/node(), $attributes, $attribute-values)}
        else $node
};

declare function syriaca:update-attribute($input-node as node()*,$attribute as xs:string,$attribute-value as xs:string)
as node()*
{
    for $node in $input-node
        return
            element {xs:QName(name($node))} {
                        $node/@*[name()!=$attribute], 
                        attribute {$attribute} {$attribute-value}, 
                        $node/node()}
};

declare function syriaca:update-secondary-xml-ids($secondary-nodes as node()*,$master-nodes as node()*,$master-id as xs:string,$iteration as xs:double?)
as node()*
{
    if ($secondary-nodes and ($master-nodes/@xml:id or $secondary-nodes/@xml:id)) then 
        for $secondary-node at $i in $secondary-nodes
            let $prefix-base := replace(($master-nodes/@xml:id|$secondary-nodes/@xml:id)[1],'[\d]+-[\d]+','')
            let $prefix := concat($prefix-base,$master-id,'-')
            let $new-node := 
                syriaca:update-attribute
                        ($secondary-node,
                        'xml:id',
                        syriaca:next-id($master-nodes/@xml:id, $prefix,$i + $iteration))
            return $new-node
                
        else $secondary-nodes
};
declare function syriaca:update-sources($input-nodes as node()*,$old-bibls as node()*,$new-bibls as node()*)
as node()*
{
(:    This function has been updated in merge-works.xql. That version should be tested here. :)
    for $node in $input-nodes
        return if ($node/descendant-or-self::*/@source) then
            if ($node/@source) then
                let $old-bibl-id := replace($node/@source,'#','')
                let $ptr := $old-bibls[@xml:id=$old-bibl-id]/ptr/@target
                let $new-source := concat('#',$new-bibls[ptr/@target=$ptr]/@xml:id)
                return syriaca:update-attribute(
                    if ($node/descendant::*/@source) then 
                        element {name($node)} {$node/@*,syriaca:update-sources($node/node(), $old-bibls, $new-bibls)}
                        else $node,
                    'source',
                    $new-source)
                else element {name($node)} {$node/@*,syriaca:update-sources($node/node(), $old-bibls, $new-bibls)}
            else $node
};
declare function syriaca:update-relation-attributes($input-nodes as node()*,$master-id,$secondary-id)
as node()*
{
    let $master-uri := concat('http://syriaca.org/person/',$master-id)
    let $master-person-id := concat('#person-',$master-id)
    let $secondary-uri := concat('http://syriaca.org/person/',$secondary-id)
    let $secondary-person-id := concat('#person-',$secondary-id)
    for $node in $input-nodes
        let $new-attributes := 
            for $attribute in $node/(@mutual|@active|@passive)
                let $attribute-value := 
                    replace
                        (replace
                            (replace
                                (replace($attribute,$master-person-id,$master-uri),
                                $secondary-person-id,
                                $master-uri),
                            $secondary-uri,
                            $master-uri), 
                            '#person\-',
                            '')
                return attribute {name($attribute)} {$attribute-value}
        return element {name($node)} {$node/@*[not(name()=('mutual','active','passive'))],$new-attributes,$node/node()}
};
 
(: MERGE FUNCTIONS :)
 declare function functx:value-union
  ( $arg1 as xs:anyAtomicType* ,
    $arg2 as xs:anyAtomicType* )  as xs:anyAtomicType* {

  distinct-values(($arg1, $arg2))
 } ;
 
declare function syriaca:normalize-space
  ( $nodes as node()* )  as node()* {

for $node in $nodes
    let $children-normalized :=
        for $child in $node/node()
        return if ($child/descendant::*) then 
                syriaca:normalize-space($child)
            else if ($child/text()) then
                element {$child/name()} {$child/@*, $child/normalize-space()}
            else $child
    return element {$node/name()} {$node/@*, $children-normalized}

 } ;

declare function syriaca:merge-master-nodes($master-nodes as node()*,$secondary-nodes as node()*,$matching-secondary-node-path as xs:string*,$elements-to-join as xs:string*) as node()*
{
    for $node in $master-nodes
        let $matching-secondary-nodes := util:eval($matching-secondary-node-path)
        let $joined-elements :=
            for $element in $elements-to-join
                return functx:distinct-deep(($node|$matching-secondary-nodes)/*[name()=$element])
(:                Due to namespace issues, might have to prefix attribute on $matching-secondary-nodes :)
        let $sources := syriaca:merge-attribute-values(($node/@source,$matching-secondary-nodes/@source), 'source')
        let $syriaca-tags-secondary := 
            for $tag in $matching-secondary-nodes/@syriaca-tags
            return attribute syriaca-tags {replace($tag,'[\s]*#syriaca-headword[\s]*','')}
        let $syriaca-tags := syriaca:merge-attribute-values(($node/@syriaca-tags,$syriaca-tags-secondary), 'syriaca-tags')
        
        return 
            element {$node/name()} {$node/@*[not(name()=('source','syriaca-tags'))],$syriaca-tags,$sources,$node/node()[not(name()=$elements-to-join)],$joined-elements}
};

declare function syriaca:merge-attribute-values($attribute-values as node()*, $attribute-name as xs:string) as node()*
{
        let $joined-values := normalize-space(string-join(distinct-values($attribute-values),' '))
        return if ($joined-values) then attribute {$attribute-name} {$joined-values} else ()
};


declare function syriaca:merge-nodes($master-nodes as node()*,$secondary-nodes as node()*,$matching-test-for-secondary-nodes as xs:string*,$elements-to-join as xs:string*,$secondary-bibls as node()*,$new-bibls as node()*) as node()*
{
    let $master-id := replace(replace($new-bibls[1]/@xml:id,'bib',''),'-[\d]+$','')
    let $secondary-unique-nodes := util:eval(concat('$secondary-nodes[not(',replace($matching-test-for-secondary-nodes,'\$node','\$master-nodes'),')]'))
    let $secondary-unique := 
        let $secondary-unique-updated-ids := 
            for $node at $i in $secondary-unique-nodes
            return syriaca:update-secondary-xml-ids($node, $master-nodes, $master-id, $i - 1)
        return syriaca:update-sources($secondary-unique-updated-ids, $secondary-bibls, $new-bibls)
    let $secondary-unique-no-headwords := 
        syriaca:remove-attribute-values($secondary-unique, 'syriaca-tags', '#syriaca-headword')
        
    let $master-merged := 
        syriaca:merge-master-nodes
            ($master-nodes,
            syriaca:update-sources($secondary-nodes, $secondary-bibls, $new-bibls), 
            concat('$secondary-nodes[',$matching-test-for-secondary-nodes,']'), 
            $elements-to-join)
    return 
        ($master-merged,$secondary-unique-no-headwords)
};

declare function syriaca:preserve-master($master-node as node()?,$secondary-node as node()?,$secondary-bibls as node()*,$new-bibls as node()*) as node()?
{
    let $secondary-node-updated := syriaca:update-sources($secondary-node, $secondary-bibls, $new-bibls)
    return 
        if ($master-node) then
            let $sources := 
                if (($master-node/@source and $secondary-node/@source) and 
                (syriaca:remove-extra-attributes($master-node, ('xml:id','source')) = syriaca:remove-extra-attributes($secondary-node, ('xml:id','source')))) 
                then
                    syriaca:merge-attribute-values(($master-node/@source,$secondary-node/@source), 'source')
                    else $master-node/@source
            return element {name($master-node)} {$master-node/@*[name()!='source'],$sources,$master-node/node()}
            else $secondary-node-updated
};

declare function syriaca:combine-dates($dates as node()*)  as node()? {
    if (count($dates) > 1) then
        let $new-dates := 
            for $date in $dates
            order by $date/@when, $date/@from
            return element date {$date/@*,$date/node()}
        
        return 
            element {name($dates[1])} {$new-dates}
    else ($dates)
 
 } ;


(: RECORD REWRITING FUNCTIONS :)
declare function syriaca:deprecate-merge-redirect($tei-root as node(),$redirect-uri as xs:string,$user as xs:string)

{
    let $secondary-uri := 
        replace($tei-root/teiHeader/fileDesc/publicationStmt/idno[@type='URI'][1], '/tei','')
    let $secondary-id := replace($secondary-uri,'http://syriaca.org/place/','')
    let $changes-old := $tei-root/teiHeader/revisionDesc/change
    let $change-id := syriaca:next-id($changes-old/@xml:id, concat('change',$secondary-id,'-'), 1)
    let $change-attribute := attribute change {concat('#',$change-id)}
    let $change :=
        element change 
            {attribute who {concat('http://syriaca.org/documentation/editors.xml#',$user)},
            attribute when {current-date()},
            attribute xml:id {$change-id},
            concat('Merged into [',$redirect-uri,'] and deprecated.')
            }
            
    let $title-old := $tei-root/teiHeader/fileDesc/titleStmt/title[@level='a']
    let $title := element title {$title-old/@*,$title-old/node(),' [deprecated]'}
    let $publication-idno-old := $tei-root/teiHeader/fileDesc/publicationStmt/idno[@type='URI']
    let $publication-idno := 
        (element idno {$publication-idno-old/@*,$change-attribute,$publication-idno-old/node()},
        element idno {attribute type {'redirect'},$change-attribute,concat($redirect-uri,'/tei')})
    let $revisionDesc-old := $tei-root/teiHeader/revisionDesc
    let $revisionDesc := element revisionDesc {
        syriaca:update-attribute($revisionDesc-old, 'status', 'deprecated')/@*,
        $change,
        $revisionDesc-old/node()}
    let $body-old := $tei-root/text/body
    let $body-text := 
            element desc {
                $change-attribute,
                attribute type {'deprecation'},
                concat('This record has been deprecated and merged into ',$redirect-uri,'.')}
    let $body := element body {$body-old/@*,$body-text,$body-old/node()}
    let $idno-old := $tei-root/text/body/listPlace/place/idno[@type='URI' and text()=$secondary-uri]
    let $idno := 
        (element idno {$idno-old/@*,$change-attribute,$idno-old/node()},
        element idno {attribute type {'redirect'},$change-attribute,$redirect-uri})
    
    let $deprecated-record := 
        <TEI xml:lang='en'>
            <teiHeader>
                <fileDesc>
                    <titleStmt>
                        {$title}
                        {$tei-root/teiHeader/fileDesc/titleStmt/node()[not(name()='title' and @level='a')]}
                    </titleStmt>
                    {$tei-root/teiHeader/fileDesc/node()[name()!='titleStmt']}
                </fileDesc>
                {$tei-root/teiHeader/node()[not(name()=('fileDesc','revisionDesc'))]}
                {$revisionDesc}
            </teiHeader>
            <text>
                <body>
                    {$body-text}
                    <listPlace>
                        <place>
                            {$tei-root/text/body/listPlace/place/@*}
                            {$tei-root/text/body/listPlace/place/(placeName|note)}
                            {$idno}
                            {$tei-root/text/body/listPlace/place/idno[not(@type='URI' and text()=$secondary-uri)]}
                            {$tei-root/text/body/listPlace/place/node()[not(name()=('placeName','note','idno'))]}
                        </place>
                    </listPlace>
                </body>
            </text>
        </TEI>
    
    return $deprecated-record
        
        
(:        (update replace $title-old with $title,:)
(:        update insert $publication-idno following $publication-idno-old, update delete $publication-idno-old,:)
(:        update replace $revisionDesc-old with $revisionDesc,:)
(:        update insert $idno following $idno-old, update delete $idno-old,:)
(:        update replace $body-old with $body):)
};

declare function syriaca:write-new-header ($header as node()*, $header-master as node()*) {
 (update insert $header following $header-master, update delete $header-master)
};

declare function syriaca:write-new-person ($person as node()*, $master-person as node()*) {
 (update insert $person following $master-person[last()], update delete $master-person)
};

declare function syriaca:write-new-relations ($relations as node()*, $relations-master as node()*, $master-person as node()*) {
 
     if ($relations) then 
        (update insert $relations following $master-person,
        if ($relations-master) then 
            update delete $relations-master
            else ())
        else ()
};

declare function syriaca:update-person-work-links ($master-uri as xs:string, $secondary-uri as xs:string, $persons as node()*, $works as node()*) {
    let $match := concat('(^|\s)',$secondary-uri,'(\s|$)')
    let $replacement := concat('$1',$master-uri,'$2')
    for $link in ($works|$persons)//@*[name()=('ref','passive','active') and matches(.,$match)]
        let $new-link := replace($link,$match,$replacement)
    return (update replace $link with $new-link)
};

(: MAIN MERGE SCRIPT :)
declare function syriaca:merge-records ($user as xs:string, $master-uri as xs:string, $secondary-uri as xs:string, $places-master-collection as node()*, $places-secondary-collection as node()*)
{
 
let $master-id := replace($master-uri,'http://syriaca.org/person/','') (:make the second parameter here more generic:)
let $master-record := $places-master-collection[text/body/listPlace/place/idno[@type='URI']=$master-uri]

let $secondary-id := replace($secondary-uri,'http://syriaca.org/person/','') (:make the second parameter here more generic:)
let $secondary-record := $places-secondary-collection[text/body/listPlace/place/idno[@type='URI']=$secondary-uri] 

let $titles-master := $master-record/teiHeader/fileDesc/titleStmt/title
let $titles-secondary := $secondary-record/teiHeader/fileDesc/titleStmt/title
let $titles := $titles-master[@level='a']

let $respStmts-master := $master-record/teiHeader/fileDesc/titleStmt/respStmt
let $respStmts-secondary := $secondary-record/teiHeader/fileDesc/titleStmt/respStmt
let $respStmts := 
    functx:distinct-deep(($respStmts-secondary,$respStmts-master))
  
let $secondary-uri-description := if ($master-uri=$secondary-uri) then 'duplicate record with same URI' else $secondary-uri
    
let $changes-secondary := 
    for $change in $secondary-record//revisionDesc/change
    return element change {$change/@*,$change/node(), concat(' [',$secondary-uri-description,']')}
let $changes-master := $master-record//revisionDesc/change

let $changes-merged := 
    for $change in ($changes-master,$changes-secondary)
    order by $change/@when descending
    return $change
    
let $change-new-id := syriaca:next-id($changes-merged/@xml:id, concat('change',$master-id,'-'), 1)
let $change-new := <change who="http://syriaca.org/documentation/editors.xml#{$user}" when="{current-date()}" xml:id="{$change-new-id}">Merged in data from [{$secondary-uri-description}].</change>

let $changes := ($change-new,$changes-merged)

let $master-place := $master-record/text/body/listPlace/place
let $secondary-place := $secondary-record/text/body/listPlace/place
let $place-combined := ($master-place,$secondary-place)

(:need to merge citations with same ptr but different citedRange? If so, delete "citedRange" in the matching test and in the fourth parameter of merge-nodes:)
let $bibl-matching-test := '.[ptr/@target]/ptr/@target=$node/ptr/@target or .[not(ptr/@target)]/node()[name()!=("citedRange","note")]=$node/node()[name()!=("citedRange","note")]'
let $bibls := 
    syriaca:merge-nodes($master-place/bibl,$secondary-place/bibl, $bibl-matching-test, ('citedRange','note'), (), $master-place/bibl)

let $test-deep-equal := './idno[@type="URI"]=$node/idno[@type="URI"]'


let $seriesStmts-master := $master-record/teiHeader/fileDesc/seriesStmt
let $seriesStmts-secondary := $secondary-record/teiHeader/fileDesc/seriesStmt
(:
let $includes-saint := matches(($seriesStmts-master|$seriesStmts-secondary)/descendant-or-self::*,'http://syriaca.org/q')
let $includes-author := matches(($seriesStmts-master|$seriesStmts-secondary)/descendant-or-self::*,'http://syriaca.org/authors')

let $biblScope-saint := 
    if ($includes-saint) then 
        <biblScope unit="vol" from="1" to="1">
            <title level="m">Qadishe: A Guide to the Syriac Saints</title>
            <idno type="URI">http://syriaca.org/q</idno>
        </biblScope>
    else ()
let $biblScope-author := 
    if ($includes-author) then
        <biblScope unit="vol" from="2" to="2">
            <title level="m">A Guide to Syriac Authors</title>
            <idno type="URI">http://syriaca.org/authors</idno>
        </biblScope>
    else ()
:) (: NEED TO ADAPT THE ABOVE FOR ANY REASON? :)
(:let $seriesStmts :=
    (<seriesStmt>
        <title level="s">The Syriac Biographical Dictionary</title>
        <editor role="general" ref="http://syriaca.org/documentation/editors.xml#dmichelson">David A. Michelson</editor>
        <editor role="associate" ref="http://syriaca.org/documentation/editors.xml#jnsaint-laurent">Jeanne-Nicole Mellon Saint-Laurent</editor>
        <editor role="associate" ref="http://syriaca.org/documentation/editors.xml#ngibson">Nathan P. Gibson</editor>
        <editor role="associate" ref="http://syriaca.org/documentation/editors.xml#dschwartz">Daniel L. Schwartz</editor>
        <respStmt>
            <resp>Edited by</resp>
            <name type="person" ref="http://syriaca.org/documentation/editors.xml#dmichelson">David A. Michelson</name>
        </respStmt>
        <respStmt>
            <resp>Edited by</resp>
            <name type="person" ref="http://syriaca.org/documentation/editors.xml#jnsaint-laurent">Jeanne-Nicole Mellon Saint-Laurent</name>
        </respStmt>
        <respStmt>
            <resp>Edited by</resp>
            <name type="person" ref="http://syriaca.org/documentation/editors.xml#ngibson">Nathan P.  Gibson</name>
        </respStmt>
        <respStmt>
            <resp>Edited by</resp>
            <name type="person" ref="http://syriaca.org/documentation/editors.xml#dschwartz">Daniel L. Schwartz</name>
        </respStmt>
        <idno type="URI">http://syriaca.org/persons</idno>
        {$biblScope-saint, $biblScope-author}
    </seriesStmt>,
    if ($includes-saint) then 
    <seriesStmt>
        <title level="s">Gateway to the Syriac Saints</title>
        <editor role="general" ref="http://syriaca.org/documentation/editors.xml#jnsaint-laurent">Jeanne-Nicole Mellon Saint-Laurent</editor>
        <editor role="general" ref="http://syriaca.org/documentation/editors.xml#dmichelson">David A.
                Michelson</editor>
        <respStmt>
            <resp>Edited by</resp>
            <name type="person" ref="http://syriaca.org/documentation/editors.xml#jnsaint-laurent">Jeanne-Nicole Mellon Saint-Laurent</name>
        </respStmt>
        <respStmt>
            <resp>Edited by</resp>
            <name type="person" ref="http://syriaca.org/documentation/editors.xml#dmichelson">David A.
                    Michelson</name>
        </respStmt>
        <idno type="URI">http://syriaca.org/saints</idno>
        <biblScope unit="vol" from="1" to="1">
            <title level="m">Qadishe: A Guide to the Syriac Saints</title>
            <idno type="URI">http://syriaca.org/q</idno>
        </biblScope>
    </seriesStmt>
    else ()):) (: NEED TO ADAPT THE ABOVE FOR ANY REASON? :)

let $placeNames := 
    syriaca:merge-nodes($master-place/placeName, 
        $secondary-place/placeName, 
        'string-join(descendant-or-self::*)=tokenize(string-join($node,","),",")', 
        (), 
        $secondary-place/bibl, 
        $bibls)
        
let $test-deep-equal-no-ids-or-sources := 'functx:is-node-in-sequence-deep-equal(syriaca:remove-extra-attributes(., ("xml:id","source")), syriaca:remove-extra-attributes($node, ("xml:id","source")))'

(:THIS IS A KEY ISSUE FOR SYRIACA <-> BQ: HOW ARE WE HANDLING THE VARIED EDITORIAL STATEMENTS CONTAINED THEREIN?:)
let $editors-master := syriaca:normalize-space($master-record/teiHeader/fileDesc/titleStmt/editor)
let $editors-secondary := syriaca:normalize-space($secondary-record/teiHeader/fileDesc/titleStmt/editor)

let $editors := 
    syriaca:merge-nodes($editors-master, 
        $editors-secondary, 
        'functx:is-node-in-sequence-deep-equal(.,$node)', 
        (), 
        $secondary-place/bibl, 
        $bibls)
        

let $titleStmt := 
    element titleStmt {$titles,
    $master-record/teiHeader/fileDesc/titleStmt/(sponsor|funder|principal),
    $editors,
    $respStmts}

let $idnos :=  (:need to tak a closer look at what this section does; not sure how to adapt for BQ :)
    (if ($master-uri=$secondary-uri) then
        $master-place/idno[@type='URI' and matches(.,'http://syriaca.org')] (:adapt the matching URI here to allow BQ? 'or' statement?  :)
    else
        (syriaca:update-attribute($master-place/idno[@type='URI' and matches(.,'http://syriaca.org')],'change',concat('#',$change-new-id)),
        syriaca:update-attribute(
            syriaca:update-attribute($secondary-place/idno[@type='URI' and matches(.,'http://syriaca.org')],'change',concat('#',$change-new-id)),
            'type',
            'deprecated'
    )),(:the below then seems to merge non-Syriaca URIs. Will need to adapt functionality to deal with BQ?:)
    syriaca:merge-nodes($master-place/idno[not(matches(.,'http://syriaca.org'))], 
        $secondary-place/idno[not(matches(.,'http://syriaca.org'))], 
        $test-deep-equal-no-ids-or-sources, 
        (), 
        $secondary-place/bibl, 
        $bibls))

let $publication-stmt-master := $master-record//publicationStmt
let $publication-stmt-secondary := $secondary-record//publicationStmt
        
let $publication-idnos-master := $publication-stmt-master/idno[@type='URI']
let $publication-idnos-secondary := $publication-stmt-secondary/idno[@type='URI']

let $publication-idnos := 
    if ($publication-idnos-master=$publication-idnos-secondary) then
        $publication-idnos-master
    else
        (syriaca:update-attribute($publication-idnos-master,'change',concat('#',$change-new-id)),
        syriaca:update-attribute(
            syriaca:update-attribute($publication-idnos-secondary,'change',concat('#',$change-new-id)),
            'type',
            'deprecated'
        ))

let $publication-stmt := 
    <publicationStmt>
        {$publication-stmt-master/authority,
        $publication-idnos,
        $publication-stmt-master/node()[name()!='authority' and name()!='idno']}
    </publicationStmt>
        
let $header-master := $master-record/teiHeader
        
let $header := 
    <teiHeader>
        <fileDesc>
            {$titleStmt}
            {($master-record/teiHeader/fileDesc/editionStmt,
            $publication-stmt,
            $master-record/teiHeader/fileDesc/sourceDesc)}
        </fileDesc>
        {($master-record/teiHeader/encodingDesc,
        $master-record/teiHeader/profileDesc,
        element revisionDesc 
            {$master-record/teiHeader/revisionDesc/@*,
            $changes})}
    </teiHeader>

(:let $abstract := syriaca:preserve-master($master-person/note[@type='abstract'], $secondary-person/note[@type='abstract'], $secondary-person/bibl, $bibls):)


let $abstract := $master-place/desc[@type='abstract']
let $desc-secondary := $secondary-place/desc[@type='abstract']
let $desc-updated := if ($desc-secondary) then element desc {$desc-secondary/@*, 'In hagiography: ', $desc-secondary/node()} else () (:fix this further:)
let $desc := 
    syriaca:update-attribute(
        syriaca:update-sources(
            syriaca:remove-extra-attributes($desc-updated, 'xml:id'), 
            $secondary-place/bibl, 
            $bibls),
        'type',
        'description') (:need to adapt to allow the multiple descriptions. Or turn non-abstract descriptions into attestations:)

(:LOCATION MERGE:)
let $events := syriaca:merge-nodes($master-place/event, 
    $secondary-place/event,
    $test-deep-equal-no-ids-or-sources, 
    (), 
    $secondary-place/bibl, 
    $bibls)

let $notes := syriaca:merge-nodes($master-place/note, 
    $secondary-place/note, 
    $test-deep-equal-no-ids-or-sources, 
    (), 
    $secondary-place/bibl, 
    $bibls)
    
let $states := syriaca:merge-nodes($master-place/state, 
    $secondary-place/state, 
    $test-deep-equal-no-ids-or-sources, 
    (), 
    $secondary-place/bibl, 
    $bibls)
    
let $relations-secondary := syriaca:update-relation-attributes($secondary-place/../listRelation/relation, $master-id, $secondary-id) (:place/78 doesn't have the listRelation element; check manual for its use:)
let $relations-master := syriaca:update-relation-attributes($master-place/../listRelation/relation, $master-id, $secondary-id)
    
let $relations := 
    if ($relations-master or $relations-secondary) then
        element listRelation {
            syriaca:merge-nodes($relations-master, 
                $relations-secondary, 
                $test-deep-equal-no-ids-or-sources, 
                (), 
                $secondary-place/bibl, 
                $bibls)
        }
    else ()
    
let $place := <place>
    {$master-place/@*}
    {($placeNames,
    $abstract,
    (:need $descs:)
    (:need $locations:)
    $events,    
    $states,    
    $notes,
    $idnos,
    (:check if we need $links, which was in the original at this point; not sure how they were generated. not in place/78:)
    $bibls)}
    </place>
    
(: Having trouble finding the right formula for matching events, etc. across records(from NGibson) :)
(: Also need to get headword selection working (from NGibson):)
return 
    <TEI xml:lang='en'>
        {$header}
        <text>
            <body>
                <listPlace>
                    {$place}
                    {$relations}
                </listPlace>
            </body>
        </text>
    </TEI>
};





(: ------------------------------------------------------------------------ :)
(: MERGE SCRIPT BODY :)
(: VARIABLES TO EDIT FOR EACH RUN :)


(: Your user id in http://syriaca.org/documentation/editors.xml :)
let $user := 'wpotter'

(:Leave the following variables blank. Uncomment the ones in the MERGE FOLDER section if you want to merge an entire folder.:)
let $records-to-merge := ''
let $master-uri-merge-folder := ''
let $secondary-uri-merge-folder := ''

    (: ------------------------------------------------------------------------ :)
    (: BEGIN MERGE FOLDER: If you want to merge an entire folder of records that have the URIs of existing records, 
 : set the $records-to-merge variable to the folder and uncomment the following lines. WILL NEED TO CHANGE TO ALLOW BQ URIs AND TO POINT TO PERSONS:)

(:let $records-to-merge := collection('/db/apps/srophe-data/data/persons/ektobe-matched-2017-06-26/')/TEI
    for $record-to-merge in $records-to-merge 
        let $master-uri-merge-folder := $record-to-merge/text/body/listPerson/person/idno[@type='URI' and matches(.,'http://syriaca\.org')]/text()
        let $secondary-uri-merge-folder := $master-uri-merge-folder:)
    (: END MERGE FOLDER :)
    (: ------------------------------------------------------------------------ :)
    
    
    
    (: Record that will be kept :)
    (: Change as needed; or use the above folder:)
        let $master-uri-merge-manual := 'http://syriaca.org/place/10'
        (: Record that will be deprecated :)
        let $secondary-uri-merge-manual := 'http://syriaca.org/place/100'
        
    let $master-uri := if ($master-uri-merge-folder) then $master-uri-merge-folder else $master-uri-merge-manual
    let $secondary-uri := if ($secondary-uri-merge-folder) then $secondary-uri-merge-folder else $secondary-uri-merge-manual
    
    let $places-master-collection := collection('/db/apps/srophe-data/data/places/tei/')/TEI (:change to places:)
    let $places-secondary-collection := if ($records-to-merge) then $records-to-merge else $places-master-collection
    let $works := collection('/db/apps/srophe-data/data/works/tei/')/TEI
    
    let $master-record := $places-master-collection[text/body/listPlace/place[idno=$master-uri]]
    let $master-collection := util:collection-name($master-record)
    let $master-filename := util:document-name($master-record)
    let $master-record-content := syriaca:merge-records($user, $master-uri, $secondary-uri, $places-master-collection, $places-secondary-collection)
    
    let $secondary-record := $places-secondary-collection[text/body/listPlace/place[idno=$secondary-uri]]
    let $secondary-collection := util:collection-name($secondary-record)
    let $secondary-filename := util:document-name($secondary-record)
    let $secondary-record-content := syriaca:deprecate-merge-redirect($secondary-record, $master-uri, $user)
    
    return 
        (xmldb:store($master-collection,$master-filename,$master-record-content),
        xmldb:store($secondary-collection,$secondary-filename,$secondary-record-content),
        if ($master-uri!=$secondary-uri) then 
            syriaca:update-person-work-links($master-uri, $secondary-uri, $places-master-collection, $works) (:check if need; if need to change:)
            else ()
        )
        
