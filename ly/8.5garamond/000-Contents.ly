\version "2.14.2"
\include "util.ly"
\header {
  title = ""
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  ragged-last-bottom = ##t
  ragged-bottom = ##t
  two-sided = ##t
  ragged-right = ##f
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #001
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = ""
  evenHeaderMarkup = ""
}
#(set-global-staff-size 23) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 23 20))) }
\markup\vspace #3.82
% \markup\fill-line \center-align {\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #50 \smallCapsOldStyle"Booklet"}}
% \markup\vspace #0.75
% \markup\fill-line \center-align {\abs-fontsize #35 \italic"of"}
% \markup\vspace #0.5
\markup\fill-line \center-align {\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #56 \smallCapsOldStyle"Christmas Carols"}}
\markup\vspace #6.18
\markup\epsfile #X #85 #"art4.eps"
% "
%\markup\vspace #20
% \markup{\abs-fontsize #12 {Sixth edition, \smallCapsOldStyle"%date% %month% %year%"}}
% \markup\vspace #0.1
% \markup{\abs-fontsize #12 "This work is free of known copyright restrictions."}
% \markup\vspace #0.1
% \markup{\abs-fontsize #12 \override #'(baseline-skip . 2.2) \wordwrap{The latest version of this book is always available at: \with-url #"http://aCollectionOfChristmasCarols.com" \italic"http://aCollectionOfChristmasCarols.com"}}
% \markup\vspace #0.1
% \markup{\abs-fontsize #12 \override #'(baseline-skip . 2.2) \wordwrap{Cover artwork, \italic Song \italic of \italic the \concat{\italic Angels ","} painted in \smallCapsOldStyle"1881" by William-Andolphe Bouguereau; downloaded from \with-url #"http://wikipaintings.org" \italic"wikipaintings.org"}}
% \markup\vspace #0.1
% \markup{\abs-fontsize #12 \override #'(baseline-skip . 2.2) \wordwrap{Inside cover artwork illustrated by Arthur Hughes, as found in \italic Christmas \italic Carols, \italic New \italic and \concat{\italic Old ";"} downloaded from \with-url #"http://www.ccel.org/b/bramley/carols/jpg-hires/0001=i.jpg" \italic"http://www.ccel.org/b/bramley/carols/jpg-hires/0001=i.jpg"}}
% \pageBreak
% \markup\vspace #0.8
%\markup\epsfile #X #85 #"Christmas.eps"
\pageBreak
%\markup\vspace #2.5
\markup\fill-line \center-align {\epsfile #X #57.3 #"art2.eps"}
%\markup\fill-line \center-align {\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #38 \smallCapsOldStyle"contents"}}
\markup\vspace #0.5

%\scale #'(0.98 . 1) 

\markup{\hspace#13.75 {\override #'(line-width . 57.3) \override #'(baseline-skip . 2.4) \override #'(word-space . 0) \column{
%CONTENTS%
}}}