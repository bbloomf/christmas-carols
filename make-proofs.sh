pushd gh-pages
echo 'Updating gh-pages repository'
git pull
popd
echo 'Updating christmas-carols repository'
if git pull
then
  cp \!full.pdf 9781475217551_content.pdf
  echo 'Adding bookmarks to 8.5x11 version'
  gs -o gh-pages/pdfs/ccc.pdf -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -f \!full.pdf pdfmarks.txt
  echo 'Extracting odd pages'
  pdftk A=\!full.pdf cat Aodd output odd.pdf
  echo 'Extracting even pages'
  pdftk A=\!full.pdf cat Aeven output even.pdf
  gs -o \!odd.pdf -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -c "[/CropBox [72 18 558 774] /PAGES pdfmark" -f odd.pdf
  gs -o \!even.pdf -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -c "[/CropBox [54 18 540 774] /PAGES pdfmark" -f even.pdf

  pdftk A=\!odd.pdf B=\!even.pdf shuffle A B output \!ccc-playbook.pdf

  if gs -o gh-pages/pdfs/ccc-playbook.pdf -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -c "[/CropBox [27 18 585 774] /PAGES pdfmark" -f \!ccc-playbook.pdf pdfmarks.txt
  then
    pushd gh-pages
    if git add pdfs/ccc-playbook.pdf pdfs/ccc.pdf
    then
      if git commit -m 'Updated PDFs'
      then
        if git push
        then
          echo 'Success!'
        else
          echo 'Error from git push --^'
        fi
      else
        echo 'Error from git commit --^'
      fi
    else
      echo 'Error from git add --^'
    fi
    popd
  else
    echo 'Error from gs --^'
  fi
  rm even.pdf odd.pdf \!even.pdf \!odd.pdf \!ccc-playbook.pdf
else
  echo 'Error from git --^'
fi