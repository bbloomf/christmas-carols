\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"In natali Domini"}}
  poet = \markup\oldStyleNum"14th Century"
  composer = \markup\oldStyleNum"Melody from Nürnberg Gesangbuch, 1544"
  arranger = \markup\oldStyleNum"Arranged by G.H. Palmer"
  tagline = \markup\concat { "from " \italic "The Cowley Carol Book" \oldStyleNum", 1919"}
}
\paper {
  %print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -15)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #044
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
#(set-global-staff-size 14) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 14 20))) }
global = {
  \key d \major
  \time 3/4
}

sopMusic = \relative c' {
  \repeat volta 2 {
    e2 g4 |
    fis2 e4 |
    d2 e4 |
    fis2 b4\rest \bar "||"
    fis2 g4 |
    a2 b4 |
    g4 fis2 |

    e2. \bar "||"
    fis2 g4 |
    a2 g4 |
    fis4 e2 |
    d2 b'4\rest \bar "||"
    a2 b4 |
    d2 cis4 |

    b2 a4 |
    b2. \bar "||" \break \once \override Score.RehearsalMark #'self-alignment-X = #LEFT \mark "Chorus"
    b2 b4 |
    a2 a4 |
    b2 g4 |
    fis2 b4\rest \bar "||"
    b2 b4 |

    a4( b) c |
    b( a) g |
    fis2. \bar "||"
    fis2 g4 |
    a2 b4 |
    g4 fis2 |
    e2. \break
  }
  
  \once \override Score.RehearsalMark #'self-alignment-X = #LEFT
  \mark "Additional verses"
  e2 g4 |
  fis2 e4 |
  d2 e4 |
  fis2 b4\rest \bar "||"
  fis2 g4 |
  a2 b4 |
  g4 fis2 |
  
  e2. \bar "||"
  fis2 g4 |
  a2 g4 |
  fis4 e2 |
  d2 b'4\rest \bar "||"
  a2 b4 |
  d2 cis4 |
  
  b2 a4 |
  b2. \bar "||"
}
sopWords = \lyricmode {
}

altoMusic = \relative c' {
  b2 d4 |
  d2 b4 |
  d2 cis4 |
  d2 s4 \bar "||"
  d2 e4 |
  fis2 g4 |
  d4 d2 |
  
  b2. \bar "||"
  d2 d4 |
  cis4( d) d |
  d2 cis4 |
  d2 s4 \bar "||"
  d2 g4 |
  fis2 fis4 |
  
  d2 d4 |
  d2. \bar "||"
  d2 g4 |
  g( fis) e |
  d2 e4 |
  d2 s4 \bar "||"
  g2 g4 |
  
  fis2 g4 |
  g( d) d |
  d2. \bar "||"
  d2 d4 |
  c2 b4 |
  d4 d2 |
  b2.
  
  
  
  b2 d4 |
  d2 b4 |
  d2 cis4 |
  d2 s4 \bar "||"
  d2 e4 |
  fis2 g4 |
  d4 d2 |
  
  b2. \bar "||"
  d2 d4 |
  cis4( d) d |
  d2 cis4 |
  d2 s4 \bar "||"
  d2 g4 |
  fis2 fis4 |
  
  d2 d4 |
  d2. \bar "||"
}
altoWords = \lyricmode {
  \dropLyricsIV
  \set stanza = #"1. "
  In na -- tá -- li Dó -- mi -- ni,
  Gau -- dent om -- nes An -- ge -- li 
  Et can -- tant cum jú -- bi -- lo_:
  Gló -- ri -- a u -- ni De -- o.
  
  \set associatedVoice = "tenors"
  Vir -- go De -- um gé -- nu -- it, 
  Vir -- go Chris -- tum pé -- pe -- rit,
  \unset associatedVoice
  Vir -- go sem -- per in -- tác -- ta.


  \set stanza = #"3. "
  Na -- tus est E -- má -- nu -- el,
  Quem præ -- dí -- xit Gá -- bri -- el,
  Tes -- tis est E -- zé -- chi -- el_:
  A Pa -- tre pro -- cés -- _ sit.
}
altoWordsII = \lyricmode {
  \dropLyricsIV
  \set stanza = #"2. "
  Nun -- ti -- á -- vit An -- ge -- lus
  Gáu -- di -- um pas -- tó -- ri -- bus, 
  Chris -- ti na -- ti -- vi -- tá -- tem
  Ma -- gnam ju -- cun -- di -- tá -- tem.
  
  \repeat unfold 21 {\skip1}
  \set stanza = #"4. "
  Chris -- tus na -- tus hó -- di -- e
  Ex Ma -- rí -- a vír -- gi -- ne,
  Non con -- cép -- tus sé -- mi -- ne
  Ap -- pá -- ru -- it hó -- di -- e_:

}
altoWordsIII = \lyricmode {
}
altoWordsIV = \lyricmode {
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
  g'2 g4 |
  a2 g4 |
  a2 g4 |
  a2 s4 \bar "||"
  b2 b4 |
  d2 d4 |
  g,4 a2 |
  
  g2. \bar "||"
  a2 d4 |
  a2 b4 |
  a2 g4 |
  fis2 s4 \bar "||"
  a2 g4 |
  b2 a4 |
  
  g2 fis4 |
  fis2. \bar "||"
  g2 d'4 |
  d2 c4 |
  b2 b4 |
  b2 s4 \bar "||"
  d2 d4 |
  
  d2 e4 |
  d( c) b |
  a2. \bar "||"
  a2 g4 |
  e( fis) g |
  g a2 |
  g2.
  
  
  
  
  
  g2 g4 |
  a2 g4 |
  a2 g4 |
  a2 s4 \bar "||"
  b2 b4 |
  d2 d4 |
  g,4 a2 |
  
  g2. \bar "||"
  a2 d4 |
  a2 b4 |
  a2 g4 |
  fis2 s4 \bar "||"
  a2 g4 |
  b2 a4 |
  
  g2 fis4 |
  fis2. \bar "||"
}
tenorWords = \lyricmode {
}

bassMusic = \relative c {
  e2 b4 |
  d2 e4 |
  fis2 e4 |
  d2 d4\rest \bar "||"
  b2 e4 |
  d2 g,4 |
  b4 d2 |
  
  e2. \bar "||"
  d2 b4 |
  fis'2 g4 |
  d4 a2 |
  d2 d4\rest \bar "||"
  fis2 e4 |
  b2 fis'4 |
  
  g2 d4 |
  b2. \bar "||"
  g'2 g4 |
  d2 d4 |
  g2 e4 |
  b2 d4\rest \bar "||"
  g,2 b4 |
  
  d2 c4 |
  g'2 g4 |
  d2. \bar "||"
  d2 b4 |
  a2 g4 |
  b4 d2 |
  e2.
  
  
  
  
  
  e2 b4 |
  d2 e4 |
  fis2 e4 |
  d2 d4\rest \bar "||"
  b2 e4 |
  d2 g,4 |
  b4 d2 |
  
  e2. \bar "||"
  d2 b4 |
  fis'2 g4 |
  d4 a2 |
  d2 d4\rest \bar "||"
  fis2 e4 |
  b2 fis'4 |
  
  g2 d4 |
  b2. \bar "||"
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
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 0) (padding . -1))} \lyricsto "sopranos" \sopWords
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 0))} \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 0))} \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 0))} \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 0))} \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 0))} \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWords
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 0) (padding . -1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 0)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
    #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 14.9 20)))
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
    \tempo 4 = 180
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
