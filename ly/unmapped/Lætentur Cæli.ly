\include "util.ly"
\version "2.14.2"
#(set-global-staff-size 15) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 15 20))) }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Lætentur Cæli"}}
  composer = \markup\oldStyleNum"Rev. Hubert Gruender, S.J."
  tagline = ""
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
  first-page-number = #183
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
global = {
  \key ees \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \oneVoice
  \partial 4 r4 |
  r4 ees g aes |
  bes2 bes~ |
  bes4 bes8 bes c[ d] ees[ c] |
  bes2 g4 g8 g |
  c4( bes aes) g |
  g2 f4 r |
  
  d'2( c4) bes |
  ees2 d4 c |
  bes2( a4.) bes8 |
  bes2 r4 bes |
  f'2 ees4( d) |
  ees2 bes |
  r1 |
  
  r2 r4 bes |
  f'2 ees4( d) |
  ees2 bes |
  r1 |
  r1 |
  ees,2( g4) bes |
  des4. c8 c2 |
  
  c2( f4) ees |
  \partial 2. d2. \repeat volta 2 {
    \partial 4 bes4~ |
    bes1~ |
    bes2 bes4 bes |
    bes1~ |
    bes2 bes4 bes8([ c] |
    d4 ees) f f |
    
    f( d) c4( bes) |
    ees( bes) aes g |
  }
  \alternative {
    {
      \partial 2. g2 f4 |
    }
    {
      c'2 f, |
    }
  }
  aes4.( g8 f2) |
  ees1 \bar "|."
  
}
sopWords = \lyricmode {
  Læ -- ten -- tur Cæ -- li __ et ex -- sul -- tet ter -- ra,
  et ex -- sul -- tet ter -- ra
  an -- te fa -- ci -- em Do -- mi -- ni.
  Læ -- ten -- tur __ cæ -- li
  
  Læ -- ten -- tur __ cæ -- li.
  An -- te fa -- ci -- em Do -- mi -- ni.
  Quo -- ni -- am ve -- nit,
  quo -- ni -- am ve -- nit, __
  quo -- ni -- am ve -- nit.
  
  ve -- nit, ve -- nit!
}

bassMusic = \relative c {
  \oneVoice
  \partial 4 bes4 |
  ees2. ees4 |
  d4( ees) f aes8 aes |
  g2( aes8[ bes]) c[ aes] |
  g4( f) ees r4 |
  r bes8 c d4 ees |
  ees2 d4 f(~ |
  
  f g a) bes |
  g( a bes) g8 ees |
  d4( f c8[ d]) ees4 |
  d2 r |
  r4 f bes aes |
  g( bes) aes g8 g |
  f4( g aes) bes |
  
  aes2 g4 r |
  r f bes aes |
  g( bes) aes g8 g |
  f4( g aes) bes |
  aes2 g |
  r4 ees2 g4 |
  bes4. aes8 aes2 |
  
  r4 f4.( g8) a4 |
  \partial 2. bes2. |
  \repeat volta 2 {
    \partial 4 bes,4( |
    ees4 d) ees f |
    ees2 bes4 g'4~(  |
    g f) g aes |
    g2 f4 bes( |
    aes g) f ees |
    
    d2 d4 r |
    r bes8[ c] d4 ees |    
  }
  \alternative {
    {
      \partial 2. ees2 d4
    }
    {
      ees2 ees
    }
  }
  d2.( f4) |
  g1 \bar "|."
}
bassWords = \lyricmode {
  Læ -- ten -- tur Cæ -- li et ex -- sul -- tet ter -- ra,
  et ex -- sul -- tet ter -- ra
  an -- te fa -- ci -- em Do -- mi -- ni.
  Læ -- ten -- tur cæ -- li et ex -- sul -- tet ter -- ra.
  
  Læ -- ten -- tur cæ -- li et ex -- sul -- tet ter -- ra.
  An -- te fa -- ci -- em Do -- mi -- ni.
  Quo -- ni -- am ve -- nit,
  quo -- ni -- am ve -- nit,
  quo -- ni -- am ve -- nit,
  quo -- ni -- am ve -- nit.
  
  ve -- nit, ve -- nit!
}

pianoRH = \relative c' {
  
}
pianoLH = \relative c' {
  
}

\score {
  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \global \sopMusic }
    >>
    \new Lyrics \with { alignBelowContext = #"women" } \lyricsto "sopranos" \sopWords
   \new Staff = men <<
      \clef bass
      \new Voice = "basses" { \global \bassMusic }
    >>
    %\new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" } \lyricsto "basses" \bassWords
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
  
}

\score {
  \unfoldRepeats

  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \global \sopMusic }
    >>
    \new Lyrics \with { alignBelowContext = #"women" } \lyricsto "sopranos" \sopWords
   \new Staff = men <<
      \clef bass
      \new Voice = "basses" { \global \bassMusic }
    >>
    %\new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \midi {
    \tempo 4 = 120
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}

