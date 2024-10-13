\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Infant Holy, Infant Lowly"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premier Pro"){ \abs-fontsize #10.5 "(W Żłobie Leży)"}}
  poet = \markup\oldStyleNum"Traditional Polish Carol"
  meter = \markup\oldStyleNum"Translated by Edith M. G. Reed (1885–1933)"
  composer = \markup\oldStyleNum"Traditional Polish Carol"
  arranger = \markup\oldStyleNum"Arranged by Edith M. G. Reed (1885–1933)"
  tagline = \markup { "from" \italic "CyberHymnal.org"}
}
\paper {
  %print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  %system-system-spacing =
  %  #'((basic-distance . 0)
  %     (minimum-distance . 0)
  %     (padding . -0.35)
  %     (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #173
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
  \key g \major
  \time 3/4
  \autoBeamOff
}

sopMusic = \relative c' {
\repeat unfold 2 {
  \partial 4 d8 d |
  g4 g fis8 g |
  a4 a b8 c |
  d4 c b8  a |
  \partial 2 g2 | \break
}

\repeat unfold 2 {
  \partial 4 g8 fis |
  e4 e a8 g |
  fis4 fis b8 a |
  \partial 2 g4 g | \break

  \partial 4 c8 b |
  a4 a b8 c |
  d4 c b8 a |
  \partial 2 g2 
}
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
\repeat unfold 2 {
  d8 d |
  d4 d d8 e |
  fis4 fis g8 g |
  g4 g fis8 fis |
  d2 |
}

\repeat unfold 2 {
    d8 d |
    c4 c c8 c |
    d4 d d8 d |
    e4 e |

    e8 e |
    fis4 fis e8 e |
    g4 g fis8 fis |
    d2 
  }
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  In -- fant ho -- ly, in -- fant low -- ly
  For His bed a cat -- tle stall;
  Ox -- en low -- ing, lit -- tle know -- ing,
  Christ the Babe, is Lord of all.
  Swift are wing -- ing an -- gels sing -- ing,
  No -- ëls ring -- ing,
  tid -- ings bring -- ing:
  Christ the Babe is Lord of all.
}
altoWordsII = \lyricmode {
  \dropLyricsV
%\markup\italic
  \set stanza = #"2. "
Flocks were sleep -- ing, shep -- herds keep -- ing
Vi -- gil till the morn -- ing new
Saw the glo -- ry, heard the sto -- ry,
Tid -- ings of a gos -- pel true.
Thus re -- joic -- ing, free from sor -- row,
Prais -- es voic -- ing greet the mor -- row:
Christ the Babe was born for all.
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c {
  \repeat unfold 2{
    d8 d |
    b'4 b d,8 d |
    d'4 d d8 c |
    d4 e d8 c |
    b2 |
  }
  \repeat unfold 2 {
    g8 g |
    g4 g a8 a |
    a4 a b8 b |
    b4 b a8 a |
    a4 a g8 g8 |
    d'4 e d8 c |
    b2 
  }
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  \repeat unfold 2 {
    d8 d |
    g4 g d8 d |
    d'4 d g,8 e |
    b4 c d8 d |
    g2 |
  }

  \repeat unfold 2 {
    b,8 b |
    c4 c a8 a |
    d4 d b8 b |
    e4 e |

    a,8 a |
    d4 d e8 c |
    b4 c d8 d |
    g,2
  }
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
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWords
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
    \tempo 4 = 95
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
