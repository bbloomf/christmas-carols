copy !full.pdf 9781475217551_content.pdf
gswin64c -o ccc.pdf -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -f !full.pdf pdfmarks.txt
pdftk A=!full.pdf cat Aodd output odd.pdf
pdftk A=!full.pdf cat Aeven output even.pdf
gswin64c -o !odd.pdf -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -c "[/CropBox [72 18 558 774] /PAGES pdfmark" -f odd.pdf
gswin64c -o !even.pdf -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -c "[/CropBox [54 18 540 774] /PAGES pdfmark" -f even.pdf

pdftk A=!odd.pdf B=!even.pdf shuffle A B output !ccc-playbook.pdf

gswin64c -o ccc-playbook.pdf -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -c "[/CropBox [27 18 585 774] /PAGES pdfmark" -f !ccc-playbook.pdf pdfmarks.txt
del even.pdf odd.pdf !even.pdf !odd.pdf !ccc-playbook.pdf