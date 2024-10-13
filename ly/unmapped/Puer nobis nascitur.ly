\version "2.14.2"
\include "util.ly"
\header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Puer nobis nascitur"}}
    poet = \markup\concat{\oldStyleNum"Words and tune (14th cent.) from " \italic"Piæ Cantiones" \oldStyleNum", 1582"}
    composer = \markup\oldStyleNum"Arranged by G.H. Palmer"
    tagline = \markup\concat { "from " \italic "The Cowley Carol Book" \oldStyleNum", 1919"}
  }\paper {
  print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -10)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 75))
  markup-system-spacing = 
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
  last-bottom-spacing = 
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -0.5)
       (stretchability . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #158
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
#(set-global-staff-size 14.8) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 14.8 20))) }
global = {
  \key d \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \once \override Score.RehearsalMark #'self-alignment-X = #LEFT
  \mark \markup\italic"To be sung in Unison."
  d4 e fis g |
  fis e \slurDotted d4( d) |
  a'4 a b cis |
  d2 d2 \bar "||"
  d4 e cis d |
  
  b a a fis |
  a g fis e |
  d( e) fis g |
  a g fis e |
  d4( d) d2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d1~|
  d4 cis d2 |
  d g~ |
  g4 fis8[ e] fis2 \bar "||"
  fis4 g e d |
  
  d2 cis4 d |
  d2 cis |
  b4 cis d2 |
  cis4 d cis2 |
  d4 b a2 \bar "|."
}
altoWords = \lyricmode {
  \set stanza = #"1. "
  Pu -- er no -- bis na -- sci -- tur
  Rec -- tor An -- ge -- lo -- rum,
  In hoc mun -- do pa -- sci -- tur
  Do -- mi -- nus do -- mi -- no -- rum,
  Do -- mi -- nus do -- mi -- no -- rum.
}
altoWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  In præ -- se -- pe po -- si -- tum
  Sub fæ -- no
  \unset ignoreMelismata
  a -- si -- no -- rum
  Co -- gno -- ve -- runt Do -- mi -- num
  Chris -- tum Re -- gem cæ -- lo -- rum,
  Chris -- tum Re -- gem cæ -- lo -- rum.
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
  Hunc He -- ro -- des ti -- mu -- it
  Ma -- gno cum tre -- mo -- re,
  In in -- fan -- tes ir -- ru -- it
  Hos cæ -- dens in fu -- ro -- re,
  Hos cæ -- dens in fu -- ro -- re.
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
  Qui na -- tus ex Ma -- ri -- a
  Di -- e ho -- di -- er -- na
  Duc nos tu -- a gra -- ti -- a
  Ad gau -- di -- a su -- per -- na,
  Ad gau -- di -- a su -- per -- na.
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  Te Sal -- va -- tor A et O
  Can -- te -- mus in cho -- ro,
  Can -- te -- mus in or -- ga -- no,
  \set ignoreMelismata = ##t
  Be -- ne -- di -- ca -- mus Do -- mi -- no,
  Be -- ne -- di -- ca -- mus Do -- mi -- no.
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  fis,4 g a b |
  a4. g8 fis2 |
  a4 d~ d g, |
  a1 \bar "||"
  b4~b a~ a |
  
  g fis e d |
  a' b a g |
  fis2. d4 |
  e b' a g |
  fis g fis2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  d2. g,4 |
  a2 d |
  fis g4 e |
  d1 \bar "||"
  b'4 e, a fis |
  
  g d a b |
  fis g a2 |
  b2.~ b4 |
  a1 |
  d2 d, \bar "|."
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
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWords
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
%6.14 \context {\Lyrics\override LyricText #'font-size = #0.9 }
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
