\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"O Tannenbaum"}}
  poet = \markup\oldStyleNum"Traditional"
  composer = \markup\oldStyleNum"German Folk Song"
  tagline = ""
}
\paper {
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
  first-page-number = #111
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
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(1 . 1)
  \override DynamicText #'X-offset = #-5
}

sopMusic = \relative c' {
  \once \override Score.RehearsalMark #'self-alignment-X = #LEFT
  \mark "Moderately"
  \partial 8 d8_\mp |
  g8.\noBeam g16 g4 a |
  b8.\noBeam b16 b4. b8 |
  a8\noBeam b c4 fis, | \break
  
  \partial 8*5 a g b8\rest \bar ":|"
  \partial 8 d8_\mf |
  d\noBeam b e4. d8 |
  d\noBeam c c4. c8 | \break
  
  c8\noBeam a d4. c8 |
  c\noBeam b b4 d, |
  g8.\noBeam_\mp g16 g4 a | \break
  
  b8.\noBeam b16 b4. b8 |
  a\noBeam b c4 fis, |
  \partial 8*5 a4 g b8\rest \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d8 |
  d8.\noBeam d16 d4 fis |
  g8.\noBeam g16 g4. g8 |
  fis8\noBeam g fis4 d |
  
  d d s8 |
  g8 |
  g\noBeam g g4. g8 |
  fis8\noBeam fis fis4. fis8 |
  
  fis\noBeam fis fis4. fis8 |
  g\noBeam g g4 d |
  d8.\noBeam d16 d4 fis |
  
  g8.\noBeam g16 g4. g8 |
  e\noBeam g g4 fis |
  fis g s8 \bar "|."
}
altoWords = \lyricmode {  
  \dropLyricsV
  \set stanza = #"1. "
  O Tan -- nen -- baum, o Tan -- nen -- baum,
  Wie treu sind dei -- ne Blät -- ter!
  Du grünst nicht nur zur Som -- mer -- zeit,
  Nein, auch im Win -- ter, wenn es schneit.
  O Tan -- nen -- baum, o Tan -- nen -- baum,
  Wie treu sind dei -- ne Blät -- ter!
}
altoWordsII = \lyricmode {
  \dropLyricsV
  \set stanza = #"2. "
  O Tan -- nen -- baum, o Tan -- nen -- baum,
  Du kannst mir sehr ge -- fal -- len!
  Wie oft hat schon zur Win -- ter -- zeit
  Ein Baum von dir mich hoch er -- freut!
  O Tan -- nen -- baum, o Tan -- nen -- baum,
  Du kannst mir sehr ge -- fal -- len!
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  O Tan -- nen -- baum, o Tan -- nen -- baum,
  Dein Kleid will mich was lehr -- en:
  Die Hoff -- nung und Be -- ständ -- ig -- keit
  Gibt Mut und Kraft zu je -- der Zeit!
  O Tan -- nen -- baum, o Tan -- nen -- baum,
  Dein Kleid will mich was lehr -- en!
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
}

tenorMusic = \relative c' {
  d8^\mp |
  b8.\noBeam b16 b4 d |
  d8.\noBeam d16 d4. d8 |
  d\noBeam d d4 a |
  
  c b s8 |
  b8^\mf |
  b\noBeam d c4. b8 |
  b\noBeam a a4. a8 |
  
  a\noBeam a a4. d8 |
  d\noBeam d d4 b |
  b8.\noBeam^\mp b16 b4 d |
  
  d8.\noBeam d16 d4. d8 |
  c\noBeam d e4 c |
  c b s8 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  d8 |
  g8.\noBeam g16 g4 d |
  g8. g16 g4. g8 |
  c8\noBeam b8\noBeam a4 d, |
  
  d g d8\rest |
  g |
  g\noBeam g g4. g8 |
  d\noBeam d d4. d8 |
  
  d\noBeam d d4. d8 |
  g\noBeam g g4 g |
  g8.\noBeam g16 g4 d |
  
  g8.\noBeam g16 g4. g8 |
  c,\noBeam b a4 d |
  d g d8\rest \bar "|."
}
bassWords = \lyricmode {

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
