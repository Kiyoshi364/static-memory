# Static Memory

This markdown is avaliable at [github.com/Kiyoshi364/static-memory](github.com/Kiyoshi364/static-memory).

## Publications

The link from _Title_ is local to the git repository.
The link from _Main Repository_ is to somewhere else,
you probably should use the link in this column to refer/cite/share.

|Type|Date (yyyy-mm)|Title|Where|Main Repository|Slides|
|:---:|:---:|:---:|:---:|:---:|:---:|
|BSc Thesis|2024-02|[From Combinators to Concatenative and Back Again](./publications/From_Combinators_to_Concatenative_and_Back_Again.pdf)|[UFRJ](https://ufrj.br/en/)|[Pantheon](http://hdl.handle.net/11422/22871)|[slides pt-BR](./publications/From_Combinators_to_Concatenative_and_Back_Again_slides.pdf)|
|Paper|2024-09|[Converting Combinators to and from Concatenative](./publications/Converting_Combinators_to_and_from_Concatenative.pdf)|[SBLP2024](https://cbsoft.sbc.org.br/2024/sblp/?lang=en)|[DOI(10.5753/sblp.2024.3460)](https://doi.org/10.5753/sblp.2024.3460)|[slides](./publications/Converting_Combinators_to_and_from_Concatenative_slides.pdf)|

## Programming Projects

|Name|Kind|Summary|Language|Main Repository|Mirrors|Last Updated|
|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
|wam|library/executable|8-bit WAM interpreter, focusing on learning the abstract machine|[C](https://en.wikipedia.org/wiki/C_(programming_language))|[github.com/Kiyoshi364/wam](https://github.com/Kiyoshi364/wam)|-|2025-03|
|[cbor.pl](https://gitlab.com/Hashi364/cbor-pl/blob/main/cbor.pl)|file library|A prolog library for reasoning about [CBOR](https://en.wikipedia.org/wiki/CBOR)|[Prolog](https://en.wikipedia.org/wiki/Prolog)|[gitlab.com/Hashi364/cbor-pl](https://gitlab.com/Hashi364/cbor-pl)|[github.com/Kiyoshi364/cbor-pl](https://github.com/Kiyoshi364/cbor-pl)|2025-02|
|[struct.pl](https://gitlab.com/Hashi364/struct-pl/blob/main/struct.pl)|file library|A prolog library for defining and using structs-like functors|[Prolog](https://en.wikipedia.org/wiki/Prolog)|[gitlab.com/Hashi364/struct-pl](https://gitlab.com/Hashi364/struct-pl)|[github.com/Kiyoshi364/struct-pl](https://github.com/Kiyoshi364/struct-pl)|2025-02|
|Yellowstone|executable|A Minecraft's Redstone inspired simulation|[Zig](https://ziglang.org/)|[github.com/Kiyoshi364/yellowstone](https://github.com/Kiyoshi364/yellowstone)|-|2024-06|
|Cutils|many executables|Some self-contained small utilities written in C|[C](https://en.wikipedia.org/wiki/C_(programming_language))|[github.com/Kiyoshi364/cutils](https://github.com/Kiyoshi364/cutils)|-|2024-06|
|crlf|executable|Converts files from linux to windows text file format or the other way around|[Zig](https://ziglang.org/)|[github.com/Kiyoshi364/crlf](https://github.com/Kiyoshi364/crlf)|-|2022-08|

## About ...

### About me

I am Daniel K Hashimoto (my first name is Daniel Kiyoshi),
a master student at [PPGI (UFRJ)](https://ppgi.ufrj.br/).
My github account is [Kiyoshi364](https://github.com/Kiyoshi364),
my gitlab is [Hashi364](https://gitlab.com/Hashi364).

If you run `$ sha1sum "mailto:$EMAIL"` (where `$EMAIL` is my email),
you should get `a638c7eafa7ba4bbe8b9cab7281113798d09da13`.

### About this markdown

I called this markdown "static-memory"
because [static memory](https://en.wikipedia.org/wiki/Static_variable)
refers to a memory segment
which holds global constants and variables
allocated at the start of the program
(in opposition to the ones allocated on the heap or the stack).
And so does this markdown.

This markdown format was inspired by [github.com/codereport/Content](https://github.com/codereport/Content).
It was generated via a Prolog script (see next subsubsection).

### About the generator

From machine-readable *data files*,
a Prolog script generates the tables in this readme
and some other data files in different formats in `serializations/`.

That's neat, right?
If you think so too,
feel free to take a look and inspire yourselffrom the generator's code.
However, I won't give you any licences for it.
Instead I encourage you to build your own generator
specially tailored for your own needs.

If you choose to make your own generator,
I suggest starting small and adding more as you go.
I also suggest spliting the code in some parts:
  * database: a human-writable and machine-readable file
    to hold your data.
    I use the script language for that,
    but you can also use any data format
    (JSON, TOML, YAML, ZIGGY, ...).
  * serializer: a code that transforms your data
    into your desired format,
    such as a markdown table, a PDF, other data formats, ...
  * (optional) checking: a code to verify that
    you have not done any silly mistakes
    while hand-editing the database file(s).
    You do not have to do this,
    but I am a bit paranoid,
    so I do it anyways.
  * script: a glue code to
    read the database(, check it) and serialize it.

Consider taking a look at the commit
`398266b20b5e119216b4116ee78b78e1757c53bb`.

## Available Serializations

Machine-readable data
about what is in this `readme.md`
is available in [serializations/](./serializations/) folder.

### [Typst](https://typst.app)

This data is also available at [serializations/static-memory.typ](./serializations/static-memory.typ).

<details><summary>Typst</summary>

```typ
#let publication = (
  (
    type: "BSc Thesis",
    date: date(year: 2024, month: 02),
    publication: link("file://./publications/From_Combinators_to_Concatenative_and_Back_Again.pdf")[From Combinators to Concatenative and Back Again],
    where: link("https://ufrj.br/en/")[UFRJ],
    main_repository: link("http://hdl.handle.net/11422/22871")[Pantheon],
    slides: link("file://./publications/From_Combinators_to_Concatenative_and_Back_Again_slides.pdf")[slides pt-BR],
  ), (
    type: "Paper",
    date: date(year: 2024, month: 09),
    publication: link("file://./publications/Converting_Combinators_to_and_from_Concatenative.pdf")[Converting Combinators to and from Concatenative],
    where: link("https://cbsoft.sbc.org.br/2024/sblp/?lang=en")[SBLP2024],
    main_repository: link("https://doi.org/10.5753/sblp.2024.3460")[DOI(10.5753/sblp.2024.3460)],
    slides: link("file://./publications/Converting_Combinators_to_and_from_Concatenative_slides.pdf")[slides],
  ), 
);
#let project = (
  (
    name: ( tag: "text", type: "text", val: "wam",),
    kind: "library/executable",
    summary: "8-bit WAM interpreter, focusing on learning the abstract machine",
    programming_language: link("https://en.wikipedia.org/wiki/C_(programming_language)")[C],
    main_repository: link("https://github.com/Kiyoshi364/wam")[github.com/Kiyoshi364/wam],
    mirrors: (),
    last_updated: date(year: 2025, month: 03),
  ), (
    name: ( tag: "link", type: "link", val: link("https://gitlab.com/Hashi364/cbor-pl/blob/main/cbor.pl")[cbor.pl],),
    kind: "file library",
    summary: "A prolog library for reasoning about [CBOR](https://en.wikipedia.org/wiki/CBOR)",
    programming_language: link("https://en.wikipedia.org/wiki/Prolog")[Prolog],
    main_repository: link("https://gitlab.com/Hashi364/cbor-pl")[gitlab.com/Hashi364/cbor-pl],
    mirrors: ( link("https://github.com/Kiyoshi364/cbor-pl")[github.com/Kiyoshi364/cbor-pl], ),
    last_updated: date(year: 2025, month: 02),
  ), (
    name: ( tag: "link", type: "link", val: link("https://gitlab.com/Hashi364/struct-pl/blob/main/struct.pl")[struct.pl],),
    kind: "file library",
    summary: "A prolog library for defining and using structs-like functors",
    programming_language: link("https://en.wikipedia.org/wiki/Prolog")[Prolog],
    main_repository: link("https://gitlab.com/Hashi364/struct-pl")[gitlab.com/Hashi364/struct-pl],
    mirrors: ( link("https://github.com/Kiyoshi364/struct-pl")[github.com/Kiyoshi364/struct-pl], ),
    last_updated: date(year: 2025, month: 02),
  ), (
    name: ( tag: "text", type: "text", val: "Yellowstone",),
    kind: "executable",
    summary: "A Minecraft's Redstone inspired simulation",
    programming_language: link("https://ziglang.org/")[Zig],
    main_repository: link("https://github.com/Kiyoshi364/yellowstone")[github.com/Kiyoshi364/yellowstone],
    mirrors: (),
    last_updated: date(year: 2024, month: 06),
  ), (
    name: ( tag: "text", type: "text", val: "Cutils",),
    kind: "many executables",
    summary: "Some self-contained small utilities written in C",
    programming_language: link("https://en.wikipedia.org/wiki/C_(programming_language)")[C],
    main_repository: link("https://github.com/Kiyoshi364/cutils")[github.com/Kiyoshi364/cutils],
    mirrors: (),
    last_updated: date(year: 2024, month: 06),
  ), (
    name: ( tag: "text", type: "text", val: "crlf",),
    kind: "executable",
    summary: "Converts files from linux to windows text file format or the other way around",
    programming_language: link("https://ziglang.org/")[Zig],
    main_repository: link("https://github.com/Kiyoshi364/crlf")[github.com/Kiyoshi364/crlf],
    mirrors: (),
    last_updated: date(year: 2022, month: 08),
  ), 
);
```
</details>

### RDF Triples ([Turtle](https://en.wikipedia.org/wiki/Turtle_(syntax)))

This data is also available at [serializations/static-memory.ttl](./serializations/static-memory.ttl).

> [!WARNING]
> I don't own a server,
> so it is hard to do [Cool URIs](https://w3.org/TR/2008/NOTE-cooluris-20081203).
> The only URIs I consider what are safe to dereference
> are the one surrounded by `<>`.
> In another words,
> DO NOT dereference URIs from the `:` prefix.

Everything with the `:` prefix is custom made.
So, there is no formal specification/ontology for them.
The `:`-prefixed predicates are pretty much stable
(but don't trust me),
they are based on the tables from this readme.
However, the `:`-prefixed things/objects are experimental.
Because of that,
consider using a structural approach
instead of relying on the resources identifiers.
For example,
find a publication via
its name (`rdfs:label`)
or its homepage (`foaf:homepage`).
([Cool URIs - Section 6.2. Reference by Description](https://w3.org/TR/2008/NOTE-cooluris-20081203#blanknodes)
has some info on what I mean by this structural approach)

<details><summary>Turtle RDF Triples</summary>

```ttl
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix : <http://github.com/Kiyoshi364/static-memory#> .

:me
  foaf:firstName "Daniel Kiyoshi"@pt-BR ;
  foaf:homepage <https://github.com/Kiyoshi364/static-memory> ;
  foaf:made
    :projects\/cbor.pl ,
    :projects\/crlf ,
    :projects\/cutils ,
    :projects\/struct.pl ,
    :projects\/wam ,
    :projects\/yellowstone ,
    :publications\/converting%20combinators%20to%20and%20from%20concatenative.pdf ,
    :publications\/from%20combinators%20to%20concatenative%20and%20back%20again.pdf ;
  foaf:mbox_sha1sum "a638c7eafa7ba4bbe8b9cab7281113798d09da13"^^xsd:hexBinary ;
  foaf:name "Daniel K Hashimoto"@pt-BR ;
  foaf:nick
    "Hashi364"^^xsd:string ,
    "Kiyoshi364"^^xsd:string ;
  foaf:publications <https://github.com/Kiyoshi364/static-memory> ;
  foaf:schoolHomepage <https://ppgi.ufrj.br/> ;
  rdf:type foaf:Person ;
  rdfs:label "Daniel K Hashimoto"@pt-BR ;
.

:projects\/cbor.pl
  :kind "file library"^^xsd:string ;
  :last_updated "2025-02"^^xsd:gYearMonth ;
  :link_link <https://gitlab.com/Hashi364/cbor-pl/blob/main/cbor.pl> ;
  :link_name "cbor.pl"^^xsd:string ;
  :main_repository_link <https://gitlab.com/Hashi364/cbor-pl> ;
  :main_repository_name "gitlab.com/Hashi364/cbor-pl"^^xsd:string ;
  :mirrors_link <https://github.com/Kiyoshi364/cbor-pl> ;
  :mirrors_name "github.com/Kiyoshi364/cbor-pl"^^xsd:string ;
  :programming_language_link <https://en.wikipedia.org/wiki/Prolog> ;
  :programming_language_name "Prolog"^^xsd:string ;
  :summary "A prolog library for reasoning about [CBOR](https://en.wikipedia.org/wiki/CBOR)"^^xsd:string ;
  foaf:homePage <https://gitlab.com/Hashi364/cbor-pl> ;
  foaf:name "cbor.pl"^^xsd:string ;
  foaf:page
    <https://github.com/Kiyoshi364/cbor-pl> ,
    <https://gitlab.com/Hashi364/cbor-pl> ;
  rdfs:label "cbor.pl"^^xsd:string ;
.

:projects\/crlf
  :kind "executable"^^xsd:string ;
  :last_updated "2022-08"^^xsd:gYearMonth ;
  :main_repository_link <https://github.com/Kiyoshi364/crlf> ;
  :main_repository_name "github.com/Kiyoshi364/crlf"^^xsd:string ;
  :programming_language_link <https://ziglang.org/> ;
  :programming_language_name "Zig"^^xsd:string ;
  :summary "Converts files from linux to windows text file format or the other way around"^^xsd:string ;
  :text "crlf"^^xsd:string ;
  foaf:homePage <https://github.com/Kiyoshi364/crlf> ;
  foaf:name "crlf"^^xsd:string ;
  foaf:page <https://github.com/Kiyoshi364/crlf> ;
  rdfs:label "crlf"^^xsd:string ;
.

:projects\/cutils
  :kind "many executables"^^xsd:string ;
  :last_updated "2024-06"^^xsd:gYearMonth ;
  :main_repository_link <https://github.com/Kiyoshi364/cutils> ;
  :main_repository_name "github.com/Kiyoshi364/cutils"^^xsd:string ;
  :programming_language_link <https://en.wikipedia.org/wiki/C_(programming_language)> ;
  :programming_language_name "C"^^xsd:string ;
  :summary "Some self-contained small utilities written in C"^^xsd:string ;
  :text "Cutils"^^xsd:string ;
  foaf:homePage <https://github.com/Kiyoshi364/cutils> ;
  foaf:name "Cutils"^^xsd:string ;
  foaf:page <https://github.com/Kiyoshi364/cutils> ;
  rdfs:label "Cutils"^^xsd:string ;
.

:projects\/struct.pl
  :kind "file library"^^xsd:string ;
  :last_updated "2025-02"^^xsd:gYearMonth ;
  :link_link <https://gitlab.com/Hashi364/struct-pl/blob/main/struct.pl> ;
  :link_name "struct.pl"^^xsd:string ;
  :main_repository_link <https://gitlab.com/Hashi364/struct-pl> ;
  :main_repository_name "gitlab.com/Hashi364/struct-pl"^^xsd:string ;
  :mirrors_link <https://github.com/Kiyoshi364/struct-pl> ;
  :mirrors_name "github.com/Kiyoshi364/struct-pl"^^xsd:string ;
  :programming_language_link <https://en.wikipedia.org/wiki/Prolog> ;
  :programming_language_name "Prolog"^^xsd:string ;
  :summary "A prolog library for defining and using structs-like functors"^^xsd:string ;
  foaf:homePage <https://gitlab.com/Hashi364/struct-pl> ;
  foaf:name "struct.pl"^^xsd:string ;
  foaf:page
    <https://github.com/Kiyoshi364/struct-pl> ,
    <https://gitlab.com/Hashi364/struct-pl> ;
  rdfs:label "struct.pl"^^xsd:string ;
.

:projects\/wam
  :kind "library/executable"^^xsd:string ;
  :last_updated "2025-03"^^xsd:gYearMonth ;
  :main_repository_link <https://github.com/Kiyoshi364/wam> ;
  :main_repository_name "github.com/Kiyoshi364/wam"^^xsd:string ;
  :programming_language_link <https://en.wikipedia.org/wiki/C_(programming_language)> ;
  :programming_language_name "C"^^xsd:string ;
  :summary "8-bit WAM interpreter, focusing on learning the abstract machine"^^xsd:string ;
  :text "wam"^^xsd:string ;
  foaf:homePage <https://github.com/Kiyoshi364/wam> ;
  foaf:name "wam"^^xsd:string ;
  foaf:page <https://github.com/Kiyoshi364/wam> ;
  rdfs:label "wam"^^xsd:string ;
.

:projects\/yellowstone
  :kind "executable"^^xsd:string ;
  :last_updated "2024-06"^^xsd:gYearMonth ;
  :main_repository_link <https://github.com/Kiyoshi364/yellowstone> ;
  :main_repository_name "github.com/Kiyoshi364/yellowstone"^^xsd:string ;
  :programming_language_link <https://ziglang.org/> ;
  :programming_language_name "Zig"^^xsd:string ;
  :summary "A Minecraft's Redstone inspired simulation"^^xsd:string ;
  :text "Yellowstone"^^xsd:string ;
  foaf:homePage <https://github.com/Kiyoshi364/yellowstone> ;
  foaf:name "Yellowstone"^^xsd:string ;
  foaf:page <https://github.com/Kiyoshi364/yellowstone> ;
  rdfs:label "Yellowstone"^^xsd:string ;
.

:publications\/converting%20combinators%20to%20and%20from%20concatenative.pdf
  :date "2024-09"^^xsd:gYearMonth ;
  :main_repository_link <https://doi.org/10.5753/sblp.2024.3460> ;
  :main_repository_name "DOI(10.5753/sblp.2024.3460)"^^xsd:string ;
  :publication_link :publications\/Converting_Combinators_to_and_from_Concatenative.pdf ;
  :publication_name "Converting Combinators to and from Concatenative"^^xsd:string ;
  :slides_link :publications\/Converting_Combinators_to_and_from_Concatenative_slides.pdf ;
  :slides_name "slides"^^xsd:string ;
  :type "Paper"^^xsd:string ;
  :where_link <https://cbsoft.sbc.org.br/2024/sblp/?lang=en> ;
  :where_name "SBLP2024"^^xsd:string ;
  foaf:homepage <https://doi.org/10.5753/sblp.2024.3460> ;
  foaf:name "Converting Combinators to and from Concatenative"^^xsd:string ;
  foaf:page <https://doi.org/10.5753/sblp.2024.3460> ;
  rdfs:label "Converting Combinators to and from Concatenative"^^xsd:string ;
.

:publications\/from%20combinators%20to%20concatenative%20and%20back%20again.pdf
  :date "2024-02"^^xsd:gYearMonth ;
  :main_repository_link <http://hdl.handle.net/11422/22871> ;
  :main_repository_name "Pantheon"^^xsd:string ;
  :publication_link :publications\/From_Combinators_to_Concatenative_and_Back_Again.pdf ;
  :publication_name "From Combinators to Concatenative and Back Again"^^xsd:string ;
  :slides_link :publications\/From_Combinators_to_Concatenative_and_Back_Again_slides.pdf ;
  :slides_name "slides pt-BR"^^xsd:string ;
  :type "BSc Thesis"^^xsd:string ;
  :where_link <https://ufrj.br/en/> ;
  :where_name "UFRJ"^^xsd:string ;
  foaf:homepage <http://hdl.handle.net/11422/22871> ;
  foaf:name "From Combinators to Concatenative and Back Again"^^xsd:string ;
  foaf:page <http://hdl.handle.net/11422/22871> ;
  rdfs:label "From Combinators to Concatenative and Back Again"^^xsd:string ;
.
```
</details>
