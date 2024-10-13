\version "2.14.2"
\include "util.ly"
\header {
  tagline = \markup \concat{ "from " \italic"The Wartburg Hymnal" \oldStyleNum", 1918, via " \italic"HymnsAndCarolsOfChristmas.com"}

    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"O Du Fröhliche"}}
    poet = \markup\oldStyleNum"Johannes Daniel Falk (1768–1826)"
    composer = \markup\oldStyleNum"Sicilian Hymn"
    tagline = \markup \concat{ "from " \italic"The Wartburg Hymnal" \oldStyleNum", 1918, via " \italic"HymnsAndCarolsOfChristmas.com"}
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

\markup\fill-line{\concat{"from " \italic"The English Carol Book, Second Series" \oldStyleNum", 1913, via " \italic"HymnsAndCarolsOfChristmas.com"}}
\markup\vspace#2










































global = {
  \key f \major
  \time 4/4
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(1 . 1)
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
  f2 e |
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

%6.14 \context {\Lyrics\override LyricText #'font-size = #0.6 }
    #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 14.5 20)))
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
    \tempo 4 = 90
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
