\version "2.14.2"
\include "util.ly"
\header {
  tagline = \markup \concat{ "from " \italic"The Wartburg Hymnal" \oldStyleNum", 1918, via " \italic"HymnsAndCarolsOfChristmas.com"}

    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"This Endris Night"}}
    composer = \markup\oldStyleNum"15th Century English"
    poet = \markup\oldStyleNum{\concat{"Adapted from " \italic"Thys endris nyzth" ", 15th Century"}}
    tagline = \markup\concat{"from " \italic"The English Carol Book, Second Series" \oldStyleNum", 1913, via " \italic"HymnsAndCarolsOfChristmas.com"}
  }
\paper {
  print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
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
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #088
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"christmas"}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine 
        \fill-line{"" \if \should-print-page-number
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine
        \if \should-print-page-number
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 15) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 15 20))) }
global = {
  \key ees \major
  \time 6/4
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(0 . 0)
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
  bes2 c4 c bes( aes) |
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
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "sopranos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
    \context {
      \Score
      %\override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      %\override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText #'X-offset = #center-on-word
    }
  }
  
  \midi {
    \tempo 4 = 150
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
