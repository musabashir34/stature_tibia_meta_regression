#!/bin/bash
# Knit article.Rmd to Word and add A4 page setup + continuous line numbers.
set -e
export LANG=C.UTF-8 LC_ALL=C.UTF-8
Rscript -e 'rmarkdown::render("article.Rmd", output_format = "word_document", quiet = TRUE)'
rm -rf _dx && mkdir _dx && cd _dx && unzip -q ../article.docx
python3 - <<'PY'
import re
d = open('word/document.xml', encoding='utf-8').read()
m = list(re.finditer(r'<w:sectPr\b[^>]*?(/>|>.*?</w:sectPr>)', d, re.S))[-1]
new = ('<w:sectPr><w:pgSz w:w="11906" w:h="16838"/><w:pgMar w:top="1440" w:right="1440" w:bottom="1440" '
       'w:left="1440" w:header="708" w:footer="708" w:gutter="0"/><w:lnNumType w:countBy="1" w:restart="continuous"/></w:sectPr>')
d = d[:m.start()] + new + d[m.end():]
# fixed column widths for Tables 1 and 2 (text width 9026 twips)
widths = [[4300, 700, 3000, 1026], [3300, 500, 900, 1600, 800, 900, 1026]]
tbls = list(re.finditer(r'<w:tbl>.*?</w:tbl>', d, re.S))
for t, w in reversed(list(zip(tbls, widths))):
    x = t.group(0)
    x = re.sub(r'<w:tblW [^>]*/>', '<w:tblW w:type="dxa" w:w="9026" /><w:tblLayout w:type="fixed" />', x, 1)
    x = re.sub(r'<w:tblGrid>.*?</w:tblGrid>', '<w:tblGrid>' + ''.join(f'<w:gridCol w:w="{v}" />' for v in w) + '</w:tblGrid>', x, 1, flags=re.S)
    d = d[:t.start()] + x + d[t.end():]
open('word/document.xml', 'w', encoding='utf-8').write(d)
PY
rm -f ../Manuscript.docx && zip -q -r ../Manuscript.docx . && cd .. && rm -rf _dx
if [ -f title_page.md ]; then pandoc title_page.md -o Title_page.docx --reference-doc=reference.docx; fi
echo "Build complete: Manuscript.docx (anonymized), Title_page.docx if title_page.md is present, figures as PDF and PNG"
