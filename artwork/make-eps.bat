rem "C:\Program Files\gs\gs9.06\bin\gswin64c" -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -c "[/CropBox [27 18 585 774] /PAGES pdfmark" -sOutputFile="!!playbook-sftpd.pdf" -f !contents.pdf !full.pdf pdfmarks.txt

rem "C:\Program Files\gs\gs9.06\bin\gswin64c" -o sftpd.pdf -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -f !full.pdf pdfmarks.txt
gswin64c -o title0.eps -dBATCH -dNOPAUSE -sDEVICE=epswrite -f title.pdf
gswin64c -o title2.eps -dBATCH -dNOPAUSE -sDEVICE=epswrite -f title2.pdf