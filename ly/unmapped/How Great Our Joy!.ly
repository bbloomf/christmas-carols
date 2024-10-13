\version "2.14.2"
\include "util.ly"
\header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"How Great Our Joy!"}}
    poet = \markup\oldStyleNum"German Carol"
    meter = \markup\oldStyleNum"Translated by Theodore Baker (1851–1934)"
    composer = \markup\oldStyleNum"German Melody"
    arranger = \markup\oldStyleNum"Arranged by Hugo Jüngst (1853–1923)"
    tagline = \markup { "from" \italic "CyberHymnal.org"}
  }\paper {
  print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #136
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
  \key bes \major
  \time 2/4
  \autoBeamOff
}

sopMusic = \relative c' {
bes'4 a8 g |
f4. f8 |
g4 a |
bes2 |
bes4 a8  g |
f4. f8 |

g4 a |
bes4. bes8 |
a4. a8 |
g2 |
a4. a8 |
g2 |

bes4 c |
d2 |
bes4 c |
d2 | \break
d4 c8 bes |
a4 g |

a4 fis |
g2 |
d'4 c8 bes |
a4 g |
a fis |
g2 \bar "|."
}

altoMusic = \relative c' {
bes'4 a8 g |
f4. f8 |
ees4 ees |
d2 |
g4 f8 ees |
d4. d8 |

ees8[ d] c[ ees] |
d4. d8 |
d4. c8 |
bes2 |
d4. c8 |
bes2 |

g'4 a |
f2 |
g4 a |
f2 |
f4 ees8 d |
c4 bes8[ ees] |

ees4 d8[ c] |
bes2 |
d4 ees8 d |
c4 bes8[ ees] |
ees4 d8[ c] |
bes2 \bar "|."
}
altoWords = {
  \dropLyricsIX
  \lyricmode {
    \set stanza = #"1. "
    While by the sheep we watched at night,
    Glad tid -- ings brought an an -- gel bright.
  }
  \set stanza = \markup\dynamic"f  "
  \lyricmode {
    How great our joy!
  }
  \set stanza = \markup\dynamic"p "
  \lyricmode {
    Great our joy!
  }
  \set stanza = \markup\dynamic"f "
  \lyricmode {
    Joy, joy, joy!
  }
  \set stanza = \markup\dynamic"p "
  \lyricmode {
    Joy, joy, joy!
  }
  \set stanza = \markup\dynamic"f  "
  \lyricmode {
    Praise we the Lord in heav’n on high!
  }
  \set stanza = \markup\dynamic"  p "
  \lyricmode {
    Praise we the Lord in heav’n on high!
  }
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
There shall be born, so he did say,
In Beth -- le -- hem a Child to -- day.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
There shall the Child lie in a stall,
This Child who shall re -- deem us all.
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
This gift of God we’ll cher -- ish well,
That ev -- er joy our hearts shall fill.
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
bes4 a8 g |
f4. bes8 |
bes4 f |
bes2 |
d4 d8 bes |
bes4. bes8 |

bes4 f |
\partial 4. f4. |\break
\partial 8 g8 |
fis4. fis8 |
g2 |
fis4. fis8 |
g2 |

d'4 f |
d2 |
d4 f |
d2 |
bes4 f8 f |
fis4 g |

c a |
g2 |
f4 fis8 g |
fis4 g |
c a |
g2 \bar "|."

}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
bes'4 a8 g |
f4. d8 |
ees[ d] c4 |
bes2 |
g'4 d8 ees |
bes4. bes8 |

ees4 f |
bes,4. g8 |
d'4. d8 |
g,2 |
d'4. d8 |
g,2 |

g'4 f |
bes2 |
g4 f |
bes2 |
bes,4 a8 bes |
d4 ees |

c d |
g,2 |
bes4 a8 g |
d'4 ees |
c d |
g,2 \bar "|."
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
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWords
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
    \tempo 4 = 120
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
