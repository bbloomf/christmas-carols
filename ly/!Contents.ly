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
  top-margin = 0.5\in
  bottom-margin = 0.75\in
  first-page-number = #1
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = ""
  evenHeaderMarkup = ""
}
#(set-global-staff-size 23) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 23 20))) }
\markup\vspace #4
\markup\fill-line \center-align {\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #50 \smallCapsOldStyle"A Collection"}}
\markup\vspace #0.75
\markup\fill-line \center-align {\abs-fontsize #35 \italic"of"}
\markup\vspace #0.5
\markup\fill-line \center-align {\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #50 \smallCapsOldStyle"Christmas Carols"}}
\markup\vspace #8.5
\markup\fill-line \center-align {\abs-fontsize #28 \smallCapsOldStyle"selected, transcribed, and edited"}
\markup\vspace #0.25
\markup\fill-line \center-align {\abs-fontsize #24 \italic"by"}
\markup\vspace #0.25
\markup\fill-line \center-align {\abs-fontsize #28 \smallCapsOldStyle"benjamin bloomfield"}
\markup\vspace #9
\markup{\abs-fontsize #12 {Fifth edition, \smallCapsOldStyle"august 2013"}}
\markup\vspace #0.1
\markup{\abs-fontsize #12 "This work is free of known copyright restrictions."}
\markup\vspace #0.1
\markup{\abs-fontsize #12 \override #'(baseline-skip . 2.2) \wordwrap{Cover artwork, \italic Song \italic of \italic the \concat{\italic Angels ","} painted in \smallCapsOldStyle"1881" by William-Andolphe Bouguereau; downloaded from \with-url #"http://wikipaintings.org" \italic"wikipaintings.org"}}
\markup\vspace #0.1
\markup{\abs-fontsize #12 \override #'(baseline-skip . 2.2) \wordwrap{Inside cover artwork illustrated by Arthur Hughes, as found in \italic Christmas \italic Carols, \italic New \italic and \concat{\italic Old ";"} downloaded from \with-url #"http://www.ccel.org/b/bramley/carols/jpg-hires/0001=i.jpg" \italic"http://www.ccel.org/b/bramley/carols/jpg-hires/0001=i.jpg"}}
\pageBreak
\markup\epsfile #X #85 #"title2.eps"
\pageBreak
%\markup{\abs-fontsize #5 \wordwrap{i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i i }}
\markup\fill-line \center-align {\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #32 \smallCapsOldStyle"preface"}}
\markup\vspace #1
\markup{\abs-fontsize #12 \override #'(baseline-skip . 2.2) \justify{Several years ago, I found an old collection of Christmas carols on the Internet, originally published in the late \oldStyleNum 1800s, called \italic Christmas \italic Carols, \italic New \italic and \concat{\italic Old ","} the music edited by Sir John Stainer and the words by Henrey Ramsden Bramley.  Just before Christmas \oldStyleNum 2010, I had this collection printed as a book through Lulu.com, and I enjoyed some of its more obscure carols enough that I thought I might combine them into a single volume containing Christmas carols from several different sources.  So in early \oldStyleNum 2011, I set about creating such a book by simply taking pages from several old collections of Christmas music and combining them into a single volume.  I thought briefly of taking the trouble of making new engravings of all the music, but it seemed an enormous task: though I had used a program called Lilypond to engrave music in the past, the amount of music I wanted to include would take many days of transcribing and proofreading, and it did not seem necessary at the time.}}
\markup\vspace #0.2
\markup{\abs-fontsize #12 \override #'(baseline-skip . 2.2) \justify{I had this collection ready (and in its third edition, the first edition having been merely a draft, and the second edition lacking \concat{\italic Gaudete ")"} in time for Christmas \oldStyleNum 2011, but after giving a few away as Christmas gifts, I decided that the book in its current form was not ideal, and worthwhile improvements could be made by making new engravings of all the music.  Thus, I have taken the trouble of transcribing everything into Lilypond for this new edition.  In this way, I have also been able to add nearly \oldStyleNum 60 more songs to the collection, including a handful of Advent hymns and two songs, \italic Ring \italic Out \italic Wild \italic Bells and \italic Auld \italic Lang \concat{\italic Syne ","} in celebration of the new year, which always begins a week after Christmas.  To make the book more affordable, I have published it through CreateSpace instead of Lulu, and in hopes that others may also find it useful, I have made it available for purchase on Amazon.com, where it should be easier to find.}}
\markup\vspace #0.2
\markup{\abs-fontsize #12 \override #'(baseline-skip . 2.2) \justify{In selecting the songs, I have tried to include all the public domain carols that are well-known, as well as those which I have found appealing.  Some songs I sought out specifically, and others I had never heard before finding them in older collections while preparing the present volume, having looked through several such books, including \italic The \italic Cowley \italic Carol \italic Book \oldStyleNum (1919), \italic The \italic Cambridge \italic Carol \italic Book \oldStyleNum (1924), the aforementioned \italic Christmas \italic Carols, \italic New \italic and \italic Old \oldStyleNum (1871), as well as the several Christmas carols found in \italic Favorite \italic Songs \italic and \italic Hymns \italic for \italic School \italic and \italic Home \oldStyleNum (1899).  In a few cases I have slightly edited the music from the source arrangement, and in rarer cases I have slightly modified the text.  In perhaps the rarest of cases, I have anonymously arranged a handful of the songs myself.}}
\markup\vspace #0.2
\markup{\abs-fontsize #12 \override #'(baseline-skip . 2.2) \justify{In ordering the songs, I have attempted to interleave the more well-known songs with those tending further toward obscurity.  However, the obscure carols seemed to outnumber those I expect to be well-known, which led to a section beginning not long after the middle of the book consisting entirely of carols of relative obscurity.  This is followed by a handful of carols of foreign origin, which are followed by a few more carols and part songs.  However, these sections are rather nebulous and songs may occasionally seem out of place within the book.}}
\markup\vspace #0.2
\markup{\abs-fontsize #12 \override #'(baseline-skip . 2.2) \justify{In laying out the music, I have tried to avoid setting lyrics for additional verses too far below the music itself, because of the difficulty involved in continually glancing back and forth between the music and the words.  Thus, some songs have the exact same music printed several times, sometimes with a chorus also doubled, though sometimes the chorus is given only once even when the verses are doubled.}}
\markup\vspace #0.2
\markup{\abs-fontsize #12 \override #'(baseline-skip . 2.2) \justify{In a few cases I have included the original foreign-language words as well as an English translation, but in other cases this was impossible, for Bramley and Stainer, while noting which texts were translations, were not so thoughtful as to include the \italic names of the original texts, and I have only been able to find the source texts for a few of them.  There are also a few foreign-language carols for which I have not included any English translation.}}
\markup\vspace #1
\markup\fill-line {"" \abs-fontsize #12 \italic"Benjamin Bloomfield"}
\markup\fill-line {"" \abs-fontsize #12 "Cincinnati, 2013"}
\pageBreak
\markup\fill-line \center-align {\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #32 \smallCapsOldStyle"contents"}}
\markup\vspace #2

%CONTENTS%