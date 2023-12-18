\version "2.24.0"
\include "util.ly"
\header {
  tagline = \markup \concat{ "from " \italic"The Wartburg Hymnal" \oldStyleNum", 1918, via " \italic"HymnsAndCarolsOfChristmas.com"}
}
\paper {
  print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #088
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"christmas"}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \if \should-print-page-number
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \if \should-print-page-number
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key ees \major
  \time 6/4
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(0 . 0)
}

sopMusic = \relative c' {
  \partial 4 ees4 |
  g2 aes4 bes2 bes4 |
  c( d) c bes2 bes4 |
  ees2 d4 c8.[( bes16] c4) aes |
  g2.~ g2 \bar""\break g4 |
  
  c2 c4 bes2 bes4 |
  aes2 aes4 g2 g4 |
  bes2 g4 aes f2 |
  ees2.~ ees2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  ees4 |
  ees2 ees4 d2 ees4 |
  ees2 aes4 f2 f4 |
  ees2 g4 aes2 c,4 |
  c2.~ c2 \bar"" c4 |
  
  c2 d4 ees2 g4 |
  f2 ees4 d2 ees4 |
  ees2 ees4 ees d2 |
  ees2.~  ees2 \bar"|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  This en -- dris night I saw a sight,
  A star as bright as day; __
  And e’er a -- mong, A maid -- en sung,
  “Lul -- lay, by by, lu -- lay.” __
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"1."
  This love -- ly la -- dy sat and sang,
  And to her Child did say, __
  “My Son, my Bro -- ther, Fa -- ther dear,
  Why liest Thou thus in hay?” __
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"2."
  “My sweet -- est bird, ’tis thus re -- quired,
  Though Thou be King ve -- ray, __
  But n’er -- the -- less I will not cease
  To sing ‘By by, lul -- lay.’” __
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3."
  The Child then spake in His talk -- ing,
  And to His mo -- ther said, __
  “Yea, I am known as Heav -- en -- King
  In crib though I be laid. __
}
altoWordsV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4."
  “For an -- gels bright down on me light;
  Thou know -- est ’tis no nay. __
  And for that sight thou mayst de -- light
  To sing, ‘By by, lul -- lay.’” __
}
altoWordsVI = \lyricmode {
  \set stanza = #" 5."
  
}
tenorMusic = \relative c' {
  g4 |
  bes2 aes4 f2 g4 |
  aes( bes) c d2 d4 |
  bes2 bes4 c2 f4 |
  ees2.~ ees2 \bar"" ees4 |
  
  ees2 aes,4 bes2 c4 |
  c( d) c b2 c4 |
  bes?2 c4 c bes( aes) |
  g2.~ g2 \bar"|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  ees4 |
  ees2 c4 bes2 ees4 |
  aes2 aes,4 bes2 aes'4 |
  g2 ees4 aes2 f4 |
  c2.~ c2 \bar"" c'4 |
  
  aes2 f4 g2 ees4 |
  f2 f4 g2 c,4 |
  g2 c4 aes bes2 |
  ees2.~ ees2 \bar"|."
}
bassWords = \lyricmode {

}

pianoRH = \relative c' {
  
}
pianoLH = \relative c' {
  
}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "sopranos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText.font-size = #1.3
  }
    \context {
      \Score
      %\override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/8)
      %\override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"This Endris Night"}}
    composer = \markup\oldStyleNum"15th Century English"
    poet = \markup\oldStyleNum{\concat{"Adapted from " \italic"Thys endris nyzth" ", 15th Century"}}
    tagline = \markup\concat{"from " \italic"The English Carol Book, Second Series" \oldStyleNum", 1913, via " \italic"HymnsAndCarolsOfChristmas.com"}
  }
}
\markup\fill-line{\concat{"from " \italic"The English Carol Book, Second Series" \oldStyleNum", 1913, via " \italic"HymnsAndCarolsOfChristmas.com"}}
\markup\vspace#2










































global = {
  \key f \major
  \time 4/4
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(1 . 1)
}

sopMusic = \relative c'' {
  c2 d |
  c4. bes8 a4( bes) |
  c2 d |
  c4. bes8 a4( bes) |
  c2 c |
  
  d e4 f |
  e2 d |
  c2. bes4\rest |
  g4.( a8) g4 a |
  bes4.( c8) bes2 |
  
  a4.( bes8) a4 bes |
  c4.( d8) c2 |
  f4( e) d( c) |
  f d c bes |
  a2 g |
  f2. bes4\rest \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  f2 f |
  f4. f8 f2 |
  f2 f |
  e4. g8 f2 |
  f2 g |
  
  g g4 a |
  g2 f |
  e2. s4 |
  e4.( f8) e4 f |
  g4.( a8) g2 |
  
  f4.( g8) f4 f |
  f2 f |
  f2 f2 |
  f4 f fis g |
  f?2 e |
  f2. s4 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  O du fröh -- li -- che, o du se -- li -- ge,
  Gna -- den -- bring -- en -- de Weih -- nachts -- zeit!
  Welt ging ver -- lor -- en,
  Christ ist ge -- bor -- en,
  Freu -- e, freu -- e dich, o Christ -- en -- heit!
}
altoWordsII = {
  \dropLyricsIX
  \set stanza = \markup{\dynamic"f  " "2. "}
  \lyricmode {
    O du fröh -- li -- che, o du se -- li -- ge,
    Gna -- den -- bring -- en -- de Weih -- nachts -- zeit!
    Christ ist er -- schien -- en,
    Uns zu ver -- söh -- nen,
    Freu -- e, freu -- e dich, o Christ -- en -- heit!
  }
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  O du fröh -- li -- che, o du se -- li -- ge,
  Gna -- den -- bring -- en -- de Weih -- nachts -- zeit!
  Himm -- li -- sche Hee -- re
  Jauch -- zen dir Eh -- re,
  Freu -- e, freu -- e dich, o Christ -- en -- heit!
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  a2 bes |
  a4. g8 f4( g) |
  a2 bes4( a) |
  g4. c8 c2 |
  c2 c |
  
  b c4 c |
  c2 b |
  g2. s4 |
  c2 c4 c  |
  c2 c |
  
  c2 c4 g |
  a4.( bes8) a2 |
  a2 bes4( c) |
  d bes ees d |
  c2 c4( bes) |
  a2. s4 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  f2 bes, |
  f'4. f8 f2 |
  f2 bes, |
  c4. e8 f4( g) |
  a2 e |
  
  d c4 f |
  g2 g, |
  c2. d4\rest |
  c2 c4 f |
  e2 e |
  
  f2 f4 f |
  f2 f2 |
  d4( c) bes( a) |
  bes bes a bes |
  c2 c |
  f2. d4\rest \bar "|."
}
bassWords = \lyricmode {

}

pianoRH = \relative c' {
  
}
pianoLH = \relative c' {
  
}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "sopranos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText.font-size = #1.3
  }

%6.14 \context {\Lyrics\override LyricText.font-size = #0.6 }
    #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20)))
    \context {
      \Score
      %\override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/8)
      %\override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"O Du Fröhliche"}}
    poet = \markup\oldStyleNum"Johannes Daniel Falk (1768–1826)"
    composer = \markup\oldStyleNum"Sicilian Hymn"
    tagline = \markup \concat{ "from " \italic"The Wartburg Hymnal" \oldStyleNum", 1918, via " \italic"HymnsAndCarolsOfChristmas.com"}
  }
}

