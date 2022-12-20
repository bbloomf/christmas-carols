pushd gh-pages
echo 'Updating gh-pages repository'
git pull
popd
echo 'Updating christmas-carols repository'
if git pull
then
  cp \!full.pdf 9781475217551_content.pdf
  cp \!full.pdf 9781475105896_content.pdf
  echo 'Adding bookmarks to 8.5x11 version'
  gs -o gh-pages/pdfs/ccc.pdf -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -f \!full.pdf pdfmarks.txt
  if gs -o gh-pages/pdfs/ccc-playbook.pdf -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -c "<</EndPage {0 eq {2 mod 0 eq {[/CropBox [72 18 558 774] /PAGE pdfmark true} {[/CropBox [54 18 540 774] /PAGE pdfmark true} ifelse}{false}ifelse}>> setpagedevice" -f \!full.pdf pdfmarks.txt
  # if pdfcrop \!full.pdf \!cropped.pdf && \
  # gs -o gh-pages/pdfs/ccc-playbook.pdf -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -f \!cropped.pdf pdfmarks.txt
  then
    # rm \!cropped.pdf
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
else
  echo 'Error from git --^'
fi