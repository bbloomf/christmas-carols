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
  first-page-number = #1
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = ""
  evenHeaderMarkup = ""
}
#(set-global-staff-size 23) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 23 20))) }
%\scale #'(0.96 . 1) 
\markup\vspace #5
\markup\fill-line \center-align {\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #50 \smallCapsOldStyle"A Collection"}}
\markup\vspace #0.75
\markup\fill-line \center-align {\abs-fontsize #35 \italic"of"}
\markup\vspace #0.5
\markup\fill-line \center-align {\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #50 \smallCapsOldStyle"Christmas Carols"}}
\markup\vspace #9.5
\markup\fill-line \center-align {\abs-fontsize #28 \smallCapsOldStyle"selected, transcribed, and edited"}
\markup\vspace #0.25
\markup\fill-line \center-align {\abs-fontsize #24 \italic"by"}
\markup\vspace #0.25
\markup\fill-line \center-align {\abs-fontsize #28 \smallCapsOldStyle"benjamin bloomfield"}
\markup\vspace #9.5 
\markup{\abs-fontsize #12 {Fifth edition, \smallCapsOldStyle"august 2013"}}
\markup\vspace #0.1
\markup{\abs-fontsize #12 "This work is free of known copyright restrictions."}
\markup\vspace #0.1
\markup{\abs-fontsize #12 \override #'(baseline-skip . 2.2) \wordwrap{Cover artwork, \italic Song \italic of \italic the \concat{\italic Angels ","} painted in \smallCapsOldStyle"1881" by William-Andolphe Bouguereau; downloaded from \with-url #"http://wikipaintings.org" \italic"wikipaintings.org"}}
\markup\vspace #0.1
\markup{\abs-fontsize #12 \override #'(baseline-skip . 2.2) \wordwrap{Inside cover artwork illustrated by Arthur Hughes, as found in \italic Christmas \italic Carols, \italic New \italic and \concat{\italic Old ";"} downloaded from \with-url #"http://www.ccel.org/b/bramley/carols/jpg-hires/0001=i.jpg" \italic"http://www.ccel.org/b/bramley/carols/jpg-hires/0001=i.jpg"}}
\pageBreak
\markup\vspace #0.8
\markup\epsfile #X #85 #"title.eps"
\pageBreak
\markup\vspace #0.8
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
\markup\vspace #0.4
\markup\fill-line \center-align {\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #30 \smallCapsOldStyle"contents"}}
\markup\vspace #0.4

%CONTENTS%
\markup{{\override #'(line-width . 41.3) \override #'(baseline-skip . 2.35) \column{
{\page-link #165 {\fill-with-pattern #0.1 #RIGHT . "’Twas in a Cave on Christmas Morn" \oldStyleNum"161"}}
{\page-link #131 {\fill-with-pattern #0.1 #RIGHT . "’Twas in the winter cold" \oldStyleNum"127"}}
{\page-link #150 {\fill-with-pattern #0.1 #RIGHT . "Ad cantus lætitiæ" \oldStyleNum"146"}}
{\page-link #16 {\fill-with-pattern #0.1 #RIGHT . "Adeste Fideles" \oldStyleNum"12"}}
{\page-link #68 {\fill-with-pattern #0.1 #RIGHT . "All my heart this night rejoices" \oldStyleNum"64"}}
{\page-link #146 {\fill-with-pattern #0.1 #RIGHT . \italic"All this night bright angels sing" \oldStyleNum"142"}}
{\page-link #83 {\fill-with-pattern #0.1 #RIGHT . "Although at Yule it Bloweth Cool" \oldStyleNum"79"}}
{\page-link #53 {\fill-with-pattern #0.1 #RIGHT . "The Angel Gabriel" \oldStyleNum"49"}}
{\page-link #81 {\fill-with-pattern #0.1 #RIGHT . "Angels from the Realms of Glory" \oldStyleNum"77"}}
{\page-link #26 {\fill-with-pattern #0.1 #RIGHT . "Angels We Have Heard on High" \oldStyleNum"22"}}
{\page-link #54 {\fill-with-pattern #0.1 #RIGHT . "Angelus ad Virginem" \oldStyleNum"50"}}
{\page-link #171 {\fill-with-pattern #0.1 #RIGHT . \italic"As Jacob with travel was weary one day" \oldStyleNum"167"}}
{\page-link #89 {\fill-with-pattern #0.1 #RIGHT . "As Lately We Watched" \oldStyleNum"85"}}
{\page-link #71 {\fill-with-pattern #0.1 #RIGHT . "As With Gladness Men of Old" \oldStyleNum"67"}}
{\page-link #205 {\fill-with-pattern #0.1 #RIGHT . "Auld Lang Syne" \oldStyleNum"201"}}
{\page-link #125 {\fill-with-pattern #0.1 #RIGHT . "Ave Jesu Deus" \oldStyleNum"121"}}
{\page-link #32 {\fill-with-pattern #0.1 #RIGHT . "Away in a Manger" \oldStyleNum"28, 28, 29"}}
{\page-link #138 {\fill-with-pattern #0.1 #RIGHT . "The Babe of Bethlehem" \oldStyleNum"134"}}
{\page-link #166 {\fill-with-pattern #0.1 #RIGHT . \italic"Behould a sely tender babe" \oldStyleNum"162"}}
{\page-link #103 {\fill-with-pattern #0.1 #RIGHT . "Blessed be that Maid Marie" \oldStyleNum"99"}}
{\page-link #80 {\fill-with-pattern #0.1 #RIGHT . "The Boar’s Head Carol" \oldStyleNum"76"}}
{\page-link #52 {\fill-with-pattern #0.1 #RIGHT . "Bring a Torch, Jeannette, Isabella!" \oldStyleNum"48"}}
{\page-link #146 {\fill-with-pattern #0.1 #RIGHT . "Carol for Christmas Day" \oldStyleNum"142"}}
{\page-link #30 {\fill-with-pattern #0.1 #RIGHT . "Carol for Christmas Eve" \oldStyleNum"26"}}
{\page-link #62 {\fill-with-pattern #0.1 #RIGHT . "A Carol for Christmas Eve" \oldStyleNum"58"}}
{\page-link #112 {\fill-with-pattern #0.1 #RIGHT . "Carol of the Bells" \oldStyleNum"108"}}
{\page-link #93 {\fill-with-pattern #0.1 #RIGHT . "Carol of the Birds" \oldStyleNum"89"}}
{\page-link #58 {\fill-with-pattern #0.1 #RIGHT . "Carol of the Shepherds" \oldStyleNum"54"}}
{\page-link #19 {\fill-with-pattern #0.1 #RIGHT . "A Child this day is born" \oldStyleNum"15"}}
{\page-link #64 {\fill-with-pattern #0.1 #RIGHT . "Christians, Awake, Salute the Happy Morn" \oldStyleNum"60"}}
{\page-link #117 {\fill-with-pattern #0.1 #RIGHT . "Christmas Bells" \oldStyleNum"113"}}
{\page-link #88 {\fill-with-pattern #0.1 #RIGHT . "Christmas Day" \oldStyleNum"84"}}
{\page-link #14 {\fill-with-pattern #0.1 #RIGHT . "Christmas is Coming" \oldStyleNum"10, 9"}}
{\page-link #196 {\fill-with-pattern #0.1 #RIGHT . "A Christmas Round" \oldStyleNum"192"}}
{\page-link #168 {\fill-with-pattern #0.1 #RIGHT . "Christmas Song" \oldStyleNum"164"}}
{\page-link #151 {\fill-with-pattern #0.1 #RIGHT . "Christmas Time is Come Again" \oldStyleNum"147"}}
{\page-link #40 {\fill-with-pattern #0.1 #RIGHT . "Christ Was Born on Christmas Day" \oldStyleNum"36"}}
{\page-link #148 {\fill-with-pattern #0.1 #RIGHT . "Chrystmasse of Olde" \oldStyleNum"144"}}
{\page-link #130 {\fill-with-pattern #0.1 #RIGHT . "Come! Tune Your Heart" \oldStyleNum"126"}}
{\page-link #58 {\fill-with-pattern #0.1 #RIGHT . \italic"Come All Ye Shepherds" \oldStyleNum"54"}}
{\page-link #8 {\fill-with-pattern #0.1 #RIGHT . "Come Thou Long Expected Jesus" \oldStyleNum"4"}}
{\page-link #121 {\fill-with-pattern #0.1 #RIGHT . "Come Ye Lofty" \oldStyleNum"117"}}
{\page-link #13 {\fill-with-pattern #0.1 #RIGHT . "Conditor alme siderum" \oldStyleNum"9"}}
{\page-link #94 {\fill-with-pattern #0.1 #RIGHT . "Congaudeat turba fidelium" \oldStyleNum"90"}}
{\page-link #126 {\fill-with-pattern #0.1 #RIGHT . "Corde Natus" \oldStyleNum"122"}}
{\page-link #66 {\fill-with-pattern #0.1 #RIGHT . "The Coventry Carol" \oldStyleNum"62, 63"}}
{\page-link #170 {\fill-with-pattern #0.1 #RIGHT . "A Cradle-Song of the Blessed Virgin" \oldStyleNum"166"}}
{\page-link #12 {\fill-with-pattern #0.1 #RIGHT . "Creator alme siderum" \oldStyleNum"8"}}
{\page-link #118 {\fill-with-pattern #0.1 #RIGHT . \italic"Dashing through the snow" \oldStyleNum"114"}}
{\page-link #152 {\fill-with-pattern #0.1 #RIGHT . "A Day, a Day of Glory" \oldStyleNum"148"}}
{\page-link #116 {\fill-with-pattern #0.1 #RIGHT . "Deck the Hall" \oldStyleNum"112"}}
}
\hspace #0.01 \path #0.1 #'((moveto 0 1.4) (lineto 0 -117.54125)) \hspace #0.01 \override #'(line-width . 41.3) \override #'(baseline-skip . 2.35) \column {
{\page-link #37 {\fill-with-pattern #0.1 #RIGHT . \italic"Ding dong ding." \oldStyleNum"33"}}
{\page-link #36 {\fill-with-pattern #0.1 #RIGHT . "Ding Dong Merrily on High" \oldStyleNum"32"}}
{\page-link #153 {\fill-with-pattern #0.1 #RIGHT . "Earth Today Rejoices" \oldStyleNum"149"}}
{\page-link #96 {\fill-with-pattern #0.1 #RIGHT . "Es ist ein Ros entsprungen" \oldStyleNum"92"}}
{\page-link #70 {\fill-with-pattern #0.1 #RIGHT . \italic"The first good joy that Mary had" \oldStyleNum"66"}}
{\page-link #20 {\fill-with-pattern #0.1 #RIGHT . "The First Noël" \oldStyleNum"16"}}
{\page-link #96 {\fill-with-pattern #0.1 #RIGHT . "Flos de radice Jesse" \oldStyleNum"92"}}
{\page-link #82 {\fill-with-pattern #0.1 #RIGHT . "The Friendly Beasts" \oldStyleNum"78"}}
{\page-link #95 {\fill-with-pattern #0.1 #RIGHT . "From Church to Church" \oldStyleNum"91"}}
{\page-link #142 {\fill-with-pattern #0.1 #RIGHT . "From far away" \oldStyleNum"138"}}
{\page-link #80 {\fill-with-pattern #0.1 #RIGHT . "From Heaven High I Come to You" \oldStyleNum"76"}}
{\page-link #176 {\fill-with-pattern #0.1 #RIGHT . "Fum, Fum, Fum" \oldStyleNum"172"}}
{\page-link #198 {\fill-with-pattern #0.1 #RIGHT . "Gaudete" \oldStyleNum"194"}}
{\page-link #108 {\fill-with-pattern #0.1 #RIGHT . "Glad Christmas Bells" \oldStyleNum"104"}}
{\page-link #198 {\fill-with-pattern #0.1 #RIGHT . "Glorious, Beauteous, Golden-Bright" \oldStyleNum"194"}}
{\page-link #78 {\fill-with-pattern #0.1 #RIGHT . "Gloucestershire Wassail" \oldStyleNum"74"}}
{\page-link #136 {\fill-with-pattern #0.1 #RIGHT . "God’s dear Son" \oldStyleNum"132"}}
{\page-link #139 {\fill-with-pattern #0.1 #RIGHT . "God Loved the World" \oldStyleNum"135"}}
{\page-link #148 {\fill-with-pattern #0.1 #RIGHT . \italic"God rest you Chrysten gentilmen" \oldStyleNum"144"}}
{\page-link #56 {\fill-with-pattern #0.1 #RIGHT . "God Rest You Merry, Gentlemen" \oldStyleNum"52"}}
{\page-link #109 {\fill-with-pattern #0.1 #RIGHT . "The Golden Carol" \oldStyleNum"105"}}
{\page-link #77 {\fill-with-pattern #0.1 #RIGHT . "Good Christian Men, Rejoice" \oldStyleNum"73"}}
{\page-link #46 {\fill-with-pattern #0.1 #RIGHT . "Good King Wenceslas" \oldStyleNum"42, 43"}}
{\page-link #60 {\fill-with-pattern #0.1 #RIGHT . \italic"Good people all, this Christmas time" \oldStyleNum"56"}}
{\page-link #134 {\fill-with-pattern #0.1 #RIGHT . \scale #'(0.96 . 1) \italic"The Great God of Heaven is come down to earth" \oldStyleNum"130"}}
{\page-link #177 {\fill-with-pattern #0.1 #RIGHT . "Hacia Belén va una burra" \oldStyleNum"173"}}
{\page-link #156 {\fill-with-pattern #0.1 #RIGHT . \scale #'(0.97 . 1) "Hail! Holy Child, Lain In An Oxen Manger" \oldStyleNum"152"}}
{\page-link #10 {\fill-with-pattern #0.1 #RIGHT . "Hark! a Herald Voice is Calling" \oldStyleNum"6"}}
{\page-link #112 {\fill-with-pattern #0.1 #RIGHT . \italic"Hark! how the bells" \oldStyleNum"108"}}
{\page-link #22 {\fill-with-pattern #0.1 #RIGHT . "Hark! the Herald Angels Sing" \oldStyleNum"18"}}
{\page-link #79 {\fill-with-pattern #0.1 #RIGHT . \italic"Here we come a wassailing" \oldStyleNum"75"}}
{\page-link #154 {\fill-with-pattern #0.1 #RIGHT . "Ho! Steward, Bid My Servants" \oldStyleNum"150"}}
{\page-link #196 {\fill-with-pattern #0.1 #RIGHT . \italic"Hodie Christus natus est" \oldStyleNum"192"}}
{\page-link #100 {\fill-with-pattern #0.1 #RIGHT . "The Holly and the Ivy" \oldStyleNum"96, 97"}}
{\page-link #140 {\fill-with-pattern #0.1 #RIGHT . "How Great Our Joy!" \oldStyleNum"136"}}
{\page-link #44 {\fill-with-pattern #0.1 #RIGHT . "Hymn for Christmas Day" \oldStyleNum"40"}}
{\page-link #195 {\fill-with-pattern #0.1 #RIGHT . \italic"If angels sung a Savior's birth" \oldStyleNum"191"}}
{\page-link #93 {\fill-with-pattern #0.1 #RIGHT . "I Heard the Bells on Christmas Day" \oldStyleNum"89"}}
{\page-link #178 {\fill-with-pattern #0.1 #RIGHT . "Il est né le divin Enfant" \oldStyleNum"174"}}
{\page-link #145 {\fill-with-pattern #0.1 #RIGHT . "In Bethlehem, that noble place" \oldStyleNum"141"}}
{\page-link #134 {\fill-with-pattern #0.1 #RIGHT . "The Incarnation" \oldStyleNum"130"}}
{\page-link #74 {\fill-with-pattern #0.1 #RIGHT . "In Dulci Jubilo" \oldStyleNum"70, 71, 72"}}
{\page-link #180 {\fill-with-pattern #0.1 #RIGHT . "Infant Holy, Infant Lowly" \oldStyleNum"176"}}
{\page-link #192 {\fill-with-pattern #0.1 #RIGHT . \italic"Infant of days, yet Lord of Life" \oldStyleNum"188"}}
{\page-link #48 {\fill-with-pattern #0.1 #RIGHT . "In natali Domini" \oldStyleNum"44"}}
{\page-link #192 {\fill-with-pattern #0.1 #RIGHT . "In Terra Pax" \oldStyleNum"188"}}
{\page-link #184 {\fill-with-pattern #0.1 #RIGHT . "In the Bleak Midwinter" \oldStyleNum"180, 182"}}
{\page-link #190 {\fill-with-pattern #0.1 #RIGHT . \italic"I saw a fair Mayden syttin and sing" \oldStyleNum"186"}}
{\page-link #69 {\fill-with-pattern #0.1 #RIGHT . "I Saw Three Ships" \oldStyleNum"65"}}
{\page-link #24 {\fill-with-pattern #0.1 #RIGHT . "It Came Upon the Midnight Clear" \oldStyleNum"20"}}
{\page-link #172 {\fill-with-pattern #0.1 #RIGHT . \italic"It was the very noon of night" \oldStyleNum"168"}}
}}}
\pageBreak
\markup\vspace#0.8
\markup{{\override #'(line-width . 41.3) \override #'(baseline-skip . 2.35) \column{
{\page-link #171 {\fill-with-pattern #0.1 #RIGHT . "Jacob's Ladder" \oldStyleNum"167"}}
{\page-link #124 {\fill-with-pattern #0.1 #RIGHT . "Jesu, hail! O God most holy" \oldStyleNum"120"}}
{\page-link #140 {\fill-with-pattern #0.1 #RIGHT . "Jesus in the Manger" \oldStyleNum"136"}}
{\page-link #82 {\fill-with-pattern #0.1 #RIGHT . \italic"Jesus our brother kind and good" \oldStyleNum"78"}}
{\page-link #23 {\fill-with-pattern #0.1 #RIGHT . "Jesus the Light of the World" \oldStyleNum"19"}}
{\page-link #118 {\fill-with-pattern #0.1 #RIGHT . "Jingle Bells" \oldStyleNum"114"}}
{\page-link #120 {\fill-with-pattern #0.1 #RIGHT . "Jolly Old Saint Nicholas" \oldStyleNum"116"}}
{\page-link #42 {\fill-with-pattern #0.1 #RIGHT . "Joseph, O Dear Joseph Mine" \oldStyleNum"38"}}
{\page-link #18 {\fill-with-pattern #0.1 #RIGHT . "Joy to the World!" \oldStyleNum"14"}}
{\page-link #179 {\fill-with-pattern #0.1 #RIGHT . "Kling Glöckchen" \oldStyleNum"175"}}
{\page-link #187 {\fill-with-pattern #0.1 #RIGHT . "Lætentur Cœli" \oldStyleNum"183"}}
{\page-link #122 {\fill-with-pattern #0.1 #RIGHT . \italic"Like silver lamps in a distant shrine" \oldStyleNum"118"}}
{\page-link #30 {\fill-with-pattern #0.1 #RIGHT . \italic"Listen Lordings unto me" \oldStyleNum"26"}}
{\page-link #97 {\fill-with-pattern #0.1 #RIGHT . "Lo, How a Rose E'er Blooming" \oldStyleNum"93"}}
{\page-link #62 {\fill-with-pattern #0.1 #RIGHT . \italic"The Lord at first had Adam made" \oldStyleNum"58"}}
{\page-link #66 {\fill-with-pattern #0.1 #RIGHT . \italic"Lullay, Thou Little Tiny Child" \oldStyleNum"62"}}
{\page-link #157 {\fill-with-pattern #0.1 #RIGHT . "Make we joy now in this fest" \oldStyleNum"153"}}
{\page-link #122 {\fill-with-pattern #0.1 #RIGHT . "The Manger Throne" \oldStyleNum"118"}}
{\page-link #27 {\fill-with-pattern #0.1 #RIGHT . "Masters in This Hall" \oldStyleNum"23"}}
{\page-link #27 {\fill-with-pattern #0.1 #RIGHT . \italic"Masters in This Hall" \oldStyleNum"23"}}
{\page-link #188 {\fill-with-pattern #0.1 #RIGHT . \italic"Methinks I see an heav’nly Host" \oldStyleNum"184"}}
{\page-link #195 {\fill-with-pattern #0.1 #RIGHT . "Milford" \oldStyleNum"191"}}
{\page-link #132 {\fill-with-pattern #0.1 #RIGHT . \italic"The moon shines bright" \oldStyleNum"128"}}
{\page-link #190 {\fill-with-pattern #0.1 #RIGHT . "Myn Lyking" \oldStyleNum"186"}}
{\page-link #166 {\fill-with-pattern #0.1 #RIGHT . "New Prince, New Pompe" \oldStyleNum"162"}}
{\page-link #181 {\fill-with-pattern #0.1 #RIGHT . "Noël Nouvelet" \oldStyleNum"177"}}
{\page-link #147 {\fill-with-pattern #0.1 #RIGHT . \italic"Now the Holly bears a berry" \oldStyleNum"143"}}
{\page-link #114 {\fill-with-pattern #0.1 #RIGHT . "O Christmas Tree" \oldStyleNum"110"}}
{\page-link #17 {\fill-with-pattern #0.1 #RIGHT . "O Come, All Ye Faithful" \oldStyleNum"13"}}
{\page-link #9 {\fill-with-pattern #0.1 #RIGHT . "O Come, Divine Messiah" \oldStyleNum"5"}}
{\page-link #98 {\fill-with-pattern #0.1 #RIGHT . "O Come, Little Children" \oldStyleNum"94"}}
{\page-link #6 {\fill-with-pattern #0.1 #RIGHT . "O Come, O Come, Emmanuel" \oldStyleNum"2"}}
{\page-link #92 {\fill-with-pattern #0.1 #RIGHT . "O Du Fröhliche" \oldStyleNum"88"}}
{\page-link #128 {\fill-with-pattern #0.1 #RIGHT . "Of the Father's Love Begotten" \oldStyleNum"124"}}
{\page-link #117 {\fill-with-pattern #0.1 #RIGHT . \italic"Oh how lovely is the evening" \oldStyleNum"113"}}
{\page-link #84 {\fill-with-pattern #0.1 #RIGHT . "O Holy Night" \oldStyleNum"80"}}
{\page-link #43 {\fill-with-pattern #0.1 #RIGHT . "O Little Town of Bethlehem" \oldStyleNum"39"}}
{\page-link #168 {\fill-with-pattern #0.1 #RIGHT . \italic"Once again O blessed time" \oldStyleNum"164"}}
{\page-link #50 {\fill-with-pattern #0.1 #RIGHT . "Once in Royal David's City" \oldStyleNum"46"}}
{\page-link #102 {\fill-with-pattern #0.1 #RIGHT . \italic"On Christmas Night all Christians Sing" \oldStyleNum"98"}}
{\page-link #10 {\fill-with-pattern #0.1 #RIGHT . "On Jordan's Bank" \oldStyleNum"6"}}
{\page-link #49 {\fill-with-pattern #0.1 #RIGHT . "On the Birthday of the Lord" \oldStyleNum"45"}}
{\page-link #28 {\fill-with-pattern #0.1 #RIGHT . \italic"On the first day of Christmas" \oldStyleNum"24"}}
{\page-link #90 {\fill-with-pattern #0.1 #RIGHT . \italic"On yesternight I saw a sight" \oldStyleNum"86"}}
{\page-link #83 {\fill-with-pattern #0.1 #RIGHT . "Orientis Partibus" \oldStyleNum"79"}}
{\page-link #115 {\fill-with-pattern #0.1 #RIGHT . "O Tannenbaum" \oldStyleNum"111"}}
{\page-link #51 {\fill-with-pattern #0.1 #RIGHT . "Past Three a Clock" \oldStyleNum"47"}}
{\page-link #72 {\fill-with-pattern #0.1 #RIGHT . "Pat-a-Pan" \oldStyleNum"68"}}
{\page-link #200 {\fill-with-pattern #0.1 #RIGHT . "Personent Hodie" \oldStyleNum"196, 197"}}
{\page-link #158 {\fill-with-pattern #0.1 #RIGHT . "Puer Natus in Bethlehem" \oldStyleNum"154"}}
{\page-link #162 {\fill-with-pattern #0.1 #RIGHT . "Puer nobis nascitur" \oldStyleNum"158"}}
{\page-link #167 {\fill-with-pattern #0.1 #RIGHT . "Quem Pastores" \oldStyleNum"163, 163"}}
}
\hspace #0.01 \path #0.1 #'((moveto 0 1.4) (lineto 0 -119.89007500000001)) \hspace #0.01 \override #'(line-width . 41.3) \override #'(baseline-skip . 2.35) \column {
{\page-link #202 {\fill-with-pattern #0.1 #RIGHT . "Ring Out, Wild Bells" \oldStyleNum"198, 200"}}
{\page-link #104 {\fill-with-pattern #0.1 #RIGHT . "Rise Up, Shepherds, and Follow" \oldStyleNum"100"}}
{\page-link #182 {\fill-with-pattern #0.1 #RIGHT . "Riu Riu Chiu" \oldStyleNum"178"}}
{\page-link #147 {\fill-with-pattern #0.1 #RIGHT . "The Sans Day Carol" \oldStyleNum"143"}}
{\page-link #111 {\fill-with-pattern #0.1 #RIGHT . \italic"Saw ye never in the twilight" \oldStyleNum"107"}}
{\page-link #44 {\fill-with-pattern #0.1 #RIGHT . \italic"See Amid the Winter's Snow" \oldStyleNum"40"}}
{\page-link #70 {\fill-with-pattern #0.1 #RIGHT . "The Seven Joys of Mary" \oldStyleNum"66"}}
{\page-link #25 {\fill-with-pattern #0.1 #RIGHT . "Shepherds! Shake Off Your Drowsy Sleep" \oldStyleNum"21"}}
{\page-link #188 {\fill-with-pattern #0.1 #RIGHT . "Shiloh" \oldStyleNum"184"}}
{\page-link #205 {\fill-with-pattern #0.1 #RIGHT . \italic"Should auld acquaintance be forgot" \oldStyleNum"201"}}
{\page-link #39 {\fill-with-pattern #0.1 #RIGHT . "Silent Night" \oldStyleNum"35"}}
{\page-link #105 {\fill-with-pattern #0.1 #RIGHT . "Sleep, Holy Babe!" \oldStyleNum"101"}}
{\page-link #160 {\fill-with-pattern #0.1 #RIGHT . "The Son of God is born for all" \oldStyleNum"156"}}
{\page-link #175 {\fill-with-pattern #0.1 #RIGHT . "Still, Still, Still" \oldStyleNum"171"}}
{\page-link #38 {\fill-with-pattern #0.1 #RIGHT . "Stille Nacht" \oldStyleNum"34"}}
{\page-link #172 {\fill-with-pattern #0.1 #RIGHT . "The Story of the Shepherd" \oldStyleNum"168"}}
{\page-link #111 {\fill-with-pattern #0.1 #RIGHT . "The Stranger Star" \oldStyleNum"107"}}
{\page-link #102 {\fill-with-pattern #0.1 #RIGHT . "The Sussex Carol" \oldStyleNum"98"}}
{\page-link #174 {\fill-with-pattern #0.1 #RIGHT . "Sweet was the song the Virgin sung" \oldStyleNum"170"}}
{\page-link #99 {\fill-with-pattern #0.1 #RIGHT . "There’s a Song in the Air!" \oldStyleNum"95"}}
{\page-link #104 {\fill-with-pattern #0.1 #RIGHT . \italic"There’s a star in the east" \oldStyleNum"100"}}
{\page-link #92 {\fill-with-pattern #0.1 #RIGHT . "This Endris Night" \oldStyleNum"88"}}
{\page-link #106 {\fill-with-pattern #0.1 #RIGHT . \italic"This is the truth sent from above" \oldStyleNum"102, 103"}}
{\page-link #196 {\fill-with-pattern #0.1 #RIGHT . "Tollite Hostias" \oldStyleNum"192"}}
{\page-link #163 {\fill-with-pattern #0.1 #RIGHT . "To us is born a little Child" \oldStyleNum"159"}}
{\page-link #164 {\fill-with-pattern #0.1 #RIGHT . "To Us This Morn a Child is Born" \oldStyleNum"160"}}
{\page-link #106 {\fill-with-pattern #0.1 #RIGHT . "The Truth from Above" \oldStyleNum"102, 103"}}
{\page-link #28 {\fill-with-pattern #0.1 #RIGHT . "The Twelve Days of Christmas" \oldStyleNum"24"}}
{\page-link #162 {\fill-with-pattern #0.1 #RIGHT . "Unto us is born a Son" \oldStyleNum"158"}}
{\page-link #37 {\fill-with-pattern #0.1 #RIGHT . "Up! Good Christen Folk and Listen" \oldStyleNum"33"}}
{\page-link #7 {\fill-with-pattern #0.1 #RIGHT . "Veni, Veni, Emmanuel" \oldStyleNum"3"}}
{\page-link #90 {\fill-with-pattern #0.1 #RIGHT . "The Virgin and Child" \oldStyleNum"86"}}
{\page-link #170 {\fill-with-pattern #0.1 #RIGHT . \italic"The virgin stills the crying" \oldStyleNum"166"}}
{\page-link #34 {\fill-with-pattern #0.1 #RIGHT . "A Virgin Unspotted" \oldStyleNum"30"}}
{\page-link #132 {\fill-with-pattern #0.1 #RIGHT . "The Waits' Song" \oldStyleNum"128"}}
{\page-link #11 {\fill-with-pattern #0.1 #RIGHT . "Wake, O Wake! with Tidings Thrilling" \oldStyleNum"7"}}
{\page-link #88 {\fill-with-pattern #0.1 #RIGHT . \italic"Wake all music's magic powers" \oldStyleNum"84"}}
{\page-link #78 {\fill-with-pattern #0.1 #RIGHT . \italic"Wassail, Wassail" \oldStyleNum"74"}}
{\page-link #79 {\fill-with-pattern #0.1 #RIGHT . "The Wassail Song" \oldStyleNum"75"}}
{\page-link #73 {\fill-with-pattern #0.1 #RIGHT . "Watchman, Tell Us of the Night" \oldStyleNum"69"}}
{\page-link #109 {\fill-with-pattern #0.1 #RIGHT . \italic"We saw a light shine out afar" \oldStyleNum"105"}}
{\page-link #110 {\fill-with-pattern #0.1 #RIGHT . "We Three Kings of Orient Are" \oldStyleNum"106"}}
{\page-link #117 {\fill-with-pattern #0.1 #RIGHT . "We Wish You a Merry Christmas" \oldStyleNum"113"}}
{\page-link #60 {\fill-with-pattern #0.1 #RIGHT . "The Wexford Carol" \oldStyleNum"56"}}
{\page-link #45 {\fill-with-pattern #0.1 #RIGHT . "What Child is This?" \oldStyleNum"41"}}
{\page-link #164 {\fill-with-pattern #0.1 #RIGHT . "When Angelick Host Entuned" \oldStyleNum"160"}}
{\page-link #93 {\fill-with-pattern #0.1 #RIGHT . \italic"Whence comes this rush of wings afar?" \oldStyleNum"89"}}
{\page-link #21 {\fill-with-pattern #0.1 #RIGHT . "When Christ Was Born of Mary Free!" \oldStyleNum"17"}}
{\page-link #140 {\fill-with-pattern #0.1 #RIGHT . \italic"While by the sheep we watched at night" \oldStyleNum"136"}}
{\page-link #59 {\fill-with-pattern #0.1 #RIGHT . "While Shepherds Watched Their Flocks" \oldStyleNum"55"}}
{\page-link #140 {\fill-with-pattern #0.1 #RIGHT . \italic"Why Most Highest art Thou lying?" \oldStyleNum"136"}}
{\page-link #72 {\fill-with-pattern #0.1 #RIGHT . \italic"Willie, get your little drum" \oldStyleNum"68"}}
}}}
