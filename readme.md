# Static Memory

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


## Triples

Machine-readable data (in [Turtle](https://en.wikipedia.org/wiki/Turtle_(syntax)))
about what is in this `readme.md`.

```ttl
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix : <http://github.com/Kiyoshi364/static-memory#> .

:me rdf:type foaf:Person.
:me rdfs:label "Daniel K Hashimoto"@pt-BR.
:me foaf:name "Daniel K Hashimoto"@pt-BR.
:me foaf:firstName "Daniel Kiyoshi"@pt-BR.
:me foaf:nick "Hashi364"^^xsd:string.
:me foaf:nick "Kiyoshi364"^^xsd:string.
:me foaf:homepage <https://github.com/Kiyoshi364/static-memory>.
:me foaf:publications <https://github.com/Kiyoshi364/static-memory>.
:me foaf:schoolHomepage <https://dcc.ufrj.br/>.
:me foaf:mbox_sha1sum "a638c7eafa7ba4bbe8b9cab7281113798d09da13"^^xsd:hexBytes.
:me foaf:made <http://hdl.handle.net/11422/22871>.
<http://hdl.handle.net/11422/22871> :type "BSc Thesis"^^xsd:string.
<http://hdl.handle.net/11422/22871> :date "2024-02"^^xsd:gYearMonth.
<http://hdl.handle.net/11422/22871> rdfs:label :publications/From_Combinators_to_Concatenative_and_Back_Again.pdf.
<http://hdl.handle.net/11422/22871> foaf:name :publications/From_Combinators_to_Concatenative_and_Back_Again.pdf.
<http://hdl.handle.net/11422/22871> :where <https://ufrj.br/en/>.
<http://hdl.handle.net/11422/22871> foaf:page <http://hdl.handle.net/11422/22871>.
<http://hdl.handle.net/11422/22871> :slides :publications/From_Combinators_to_Concatenative_and_Back_Again_slides.pdf.
:me foaf:made <https://doi.org/10.5753/sblp.2024.3460>.
<https://doi.org/10.5753/sblp.2024.3460> :type "Paper"^^xsd:string.
<https://doi.org/10.5753/sblp.2024.3460> :date "2024-09"^^xsd:gYearMonth.
<https://doi.org/10.5753/sblp.2024.3460> rdfs:label :publications/Converting_Combinators_to_and_from_Concatenative.pdf.
<https://doi.org/10.5753/sblp.2024.3460> foaf:name :publications/Converting_Combinators_to_and_from_Concatenative.pdf.
<https://doi.org/10.5753/sblp.2024.3460> :where <https://cbsoft.sbc.org.br/2024/sblp/?lang=en>.
<https://doi.org/10.5753/sblp.2024.3460> foaf:page <https://doi.org/10.5753/sblp.2024.3460>.
<https://doi.org/10.5753/sblp.2024.3460> :slides :publications/Converting_Combinators_to_and_from_Concatenative_slides.pdf.
:me foaf:made <https://github.com/Kiyoshi364/wam>.
<https://github.com/Kiyoshi364/wam> rdfs:label "wam"^^xsd:string.
<https://github.com/Kiyoshi364/wam> foaf:name "wam"^^xsd:string.
<https://github.com/Kiyoshi364/wam> :kind "library/executable"^^xsd:string.
<https://github.com/Kiyoshi364/wam> :summary "8-bit WAM interpreter, focusing on learning the abstract machine"^^xsd:string.
<https://github.com/Kiyoshi364/wam> :programming_language <https://en.wikipedia.org/wiki/C_(programming_language)>.
<https://github.com/Kiyoshi364/wam> foaf:homePage <https://github.com/Kiyoshi364/wam>.
<https://github.com/Kiyoshi364/wam> foaf:page <https://github.com/Kiyoshi364/wam>.
<https://github.com/Kiyoshi364/wam> :last_updated "2025-03"^^xsd:gYearMonth.
:me foaf:made <https://gitlab.com/Hashi364/cbor-pl>.
<https://gitlab.com/Hashi364/cbor-pl> rdfs:label <https://gitlab.com/Hashi364/cbor-pl/blob/main/cbor.pl>.
<https://gitlab.com/Hashi364/cbor-pl> foaf:name <https://gitlab.com/Hashi364/cbor-pl/blob/main/cbor.pl>.
<https://gitlab.com/Hashi364/cbor-pl> :kind "file library"^^xsd:string.
<https://gitlab.com/Hashi364/cbor-pl> :summary "A prolog library for reasoning about [CBOR](https://en.wikipedia.org/wiki/CBOR)"^^xsd:string.
<https://gitlab.com/Hashi364/cbor-pl> :programming_language <https://en.wikipedia.org/wiki/Prolog>.
<https://gitlab.com/Hashi364/cbor-pl> foaf:homePage <https://gitlab.com/Hashi364/cbor-pl>.
<https://gitlab.com/Hashi364/cbor-pl> foaf:page <https://gitlab.com/Hashi364/cbor-pl>.
<https://gitlab.com/Hashi364/cbor-pl> foaf:page <https://github.com/Kiyoshi364/cbor-pl>.
<https://gitlab.com/Hashi364/cbor-pl> :last_updated "2025-02"^^xsd:gYearMonth.
:me foaf:made <https://gitlab.com/Hashi364/struct-pl>.
<https://gitlab.com/Hashi364/struct-pl> rdfs:label <https://gitlab.com/Hashi364/struct-pl/blob/main/struct.pl>.
<https://gitlab.com/Hashi364/struct-pl> foaf:name <https://gitlab.com/Hashi364/struct-pl/blob/main/struct.pl>.
<https://gitlab.com/Hashi364/struct-pl> :kind "file library"^^xsd:string.
<https://gitlab.com/Hashi364/struct-pl> :summary "A prolog library for defining and using structs-like functors"^^xsd:string.
<https://gitlab.com/Hashi364/struct-pl> :programming_language <https://en.wikipedia.org/wiki/Prolog>.
<https://gitlab.com/Hashi364/struct-pl> foaf:homePage <https://gitlab.com/Hashi364/struct-pl>.
<https://gitlab.com/Hashi364/struct-pl> foaf:page <https://gitlab.com/Hashi364/struct-pl>.
<https://gitlab.com/Hashi364/struct-pl> foaf:page <https://github.com/Kiyoshi364/struct-pl>.
<https://gitlab.com/Hashi364/struct-pl> :last_updated "2025-02"^^xsd:gYearMonth.
:me foaf:made <https://github.com/Kiyoshi364/yellowstone>.
<https://github.com/Kiyoshi364/yellowstone> rdfs:label "Yellowstone"^^xsd:string.
<https://github.com/Kiyoshi364/yellowstone> foaf:name "Yellowstone"^^xsd:string.
<https://github.com/Kiyoshi364/yellowstone> :kind "executable"^^xsd:string.
<https://github.com/Kiyoshi364/yellowstone> :summary "A Minecraft's Redstone inspired simulation"^^xsd:string.
<https://github.com/Kiyoshi364/yellowstone> :programming_language <https://ziglang.org/>.
<https://github.com/Kiyoshi364/yellowstone> foaf:homePage <https://github.com/Kiyoshi364/yellowstone>.
<https://github.com/Kiyoshi364/yellowstone> foaf:page <https://github.com/Kiyoshi364/yellowstone>.
<https://github.com/Kiyoshi364/yellowstone> :last_updated "2024-06"^^xsd:gYearMonth.
:me foaf:made <https://github.com/Kiyoshi364/cutils>.
<https://github.com/Kiyoshi364/cutils> rdfs:label "Cutils"^^xsd:string.
<https://github.com/Kiyoshi364/cutils> foaf:name "Cutils"^^xsd:string.
<https://github.com/Kiyoshi364/cutils> :kind "many executables"^^xsd:string.
<https://github.com/Kiyoshi364/cutils> :summary "Some self-contained small utilities written in C"^^xsd:string.
<https://github.com/Kiyoshi364/cutils> :programming_language <https://en.wikipedia.org/wiki/C_(programming_language)>.
<https://github.com/Kiyoshi364/cutils> foaf:homePage <https://github.com/Kiyoshi364/cutils>.
<https://github.com/Kiyoshi364/cutils> foaf:page <https://github.com/Kiyoshi364/cutils>.
<https://github.com/Kiyoshi364/cutils> :last_updated "2024-06"^^xsd:gYearMonth.
:me foaf:made <https://github.com/Kiyoshi364/crlf>.
<https://github.com/Kiyoshi364/crlf> rdfs:label "crlf"^^xsd:string.
<https://github.com/Kiyoshi364/crlf> foaf:name "crlf"^^xsd:string.
<https://github.com/Kiyoshi364/crlf> :kind "executable"^^xsd:string.
<https://github.com/Kiyoshi364/crlf> :summary "Converts files from linux to windows text file format or the other way around"^^xsd:string.
<https://github.com/Kiyoshi364/crlf> :programming_language <https://ziglang.org/>.
<https://github.com/Kiyoshi364/crlf> foaf:homePage <https://github.com/Kiyoshi364/crlf>.
<https://github.com/Kiyoshi364/crlf> foaf:page <https://github.com/Kiyoshi364/crlf>.
<https://github.com/Kiyoshi364/crlf> :last_updated "2022-08"^^xsd:gYearMonth.
```
