#!/bin/bash
printf "%s\n" "Converting Markdown to PDF"
pandoc \
  --standalone \
  --template=eisvogel \
  --citeproc \
  --listings \
  --bibliography=strategic_messaging.bib \
  keyinfo.yaml \
  strategic_text.md \
  -o strategic_text.pdf

printf "%s\n" "Finished"
