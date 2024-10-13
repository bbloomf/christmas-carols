\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Silent Night"}}
  poet = \markup\oldStyleNum"Translated by John Freeman Young (1820–1885)"
  composer = \markup\oldStyleNum"Franz Gruber (1787–1863)"
  tagline = \markup { "from" \concat{\italic "Christmas Carols and Hymns for School and Choir" \oldStyleNum", 1910"}}
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
  first-page-number = #035
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
  \time 6/8
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \tempo \markup\italic"Tranquillo" 8 = 90
  f8.[ g16] f8 d4. |
  f8.[ g16] f8 d4. |
  c'8.[ b16] c8 a4. | \break
  bes8.[ a16] bes8 f4. |
  
  g4 g8 bes8.[ a16] g8 |
  f8.\noBeam g16\noBeam f8 d4 bes'8\rest | \break
  \slurDotted g4 g8\noBeam bes8.\noBeam( a16\noBeam) g8 |
  f8.\noBeam( g16\noBeam) f8 d4 bes'8\rest |
  
  \slurSolid c4 c8\noBeam ees8.\noBeam c16\noBeam a8 | \break
  bes4.( d4) bes8\rest |
  bes8[ f] d8 f8.\noBeam ees16\noBeam c8 |
  bes4.~bes4 bes'8\rest \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d8.[ ees16] d8 bes4. |
  d8.[ ees16] d8 bes4. |
  ees8.[ d16] ees8 c4. |
  d8.[ c16] d8 d4. |
  
  ees4 ees8 g8.[ f16] ees8 |
  d8.\noBeam ees16\noBeam d8\noBeam bes4 s8 |
  \slurDotted ees4 ees8\noBeam g8.\noBeam( f16\noBeam) ees8 |
  d8.\noBeam( ees16\noBeam) d8 bes4 s8 |
  
  \slurSolid ees4 ees8 c8.\noBeam ees16\noBeam c8 |
  d4.( f4) s8 |
  d4 bes8 d8.\noBeam c16\noBeam a8 |
  bes4.~ bes4 s8 \bar "|."
}
altoWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  Si -- _ lent night!
  Ho -- _ ly night!
  All __ _ is calm,
  all __ _ is bright.
  Round yon Vir -- _ gin
  Moth -- er and Child,
  Ho -- ly In -- fant, so ten -- der and mild,
  Sleep in heav -- en -- ly peace, __ _
  Sleep _ in heav -- en -- ly peace. __ _
}
altoWordsII = {
  \set stanza = \markup\concat{\dynamic"p   " "2. "}
  \lyricmode {
    \set ignoreMelismata = ##t
    Si -- _ lent night!  Ho -- _ ly night!
    Shep -- _ herds quake at __ _ the sight;
    Glo -- ries stream _ from heav -- en a -- far,
    Heav’n -- ly hosts __ _ sing Al -- le -- lu -- ia;
  }
  \set stanza = \markup\dynamic"mf  "
  \lyricmode {
    Christ, the Sav -- ior is born! __ _
  }
  \set stanza = \markup\dynamic"pp      "
  \lyricmode {
    Christ, _ the Sav -- ior is born! __ _
  }
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  Si -- _ lent night!  Ho -- _ ly night!
  Son __ _ of God, love’s __ _ pure light!
  Ra -- diant beams _ from Thy ho -- ly face,
  With the dawn of re -- deem -- _ ing grace,
  Je -- sus, Lord, at Thy birth! __ _
  Je -- _ sus, Lord, at Thy birth! __ _
}
altoWordsIV = \lyricmode {
\set ignoreMelismata = ##t
%\markup\italic 
}

tenorMusic = \relative c' {
  bes4 bes8 f4. |
  bes4 bes8 f4. |
  a4 a8 f4. |
  f4 f8 bes4. |
  
  bes4 bes8 g8.[ a16] bes8 |
  bes8.\noBeam bes16\noBeam bes8 f4 s8 |
  \slurDotted bes4 bes8 g8.\noBeam( a16\noBeam) bes8 |
  bes8.\noBeam( bes16\noBeam) bes8 f4 s8 |
  
  a4 a8\noBeam a8.\noBeam a16\noBeam f8 |
  \slurSolid f4.( bes4) s8 |
  f4 f8\noBeam f8.\noBeam f16\noBeam ees8 |
  d4.~d4 s8 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  bes4 bes8 bes4. |
  bes4 bes8 bes4. |
  f'4 f8 f4. |
  bes,4 bes8 bes4. |
  
  ees4 ees8 ees4 ees8 |
  bes8.\noBeam bes16\noBeam bes8\noBeam bes4 d8\rest |
  \slurDotted ees4 ees8\noBeam ees8.\noBeam( ees16\noBeam) ees8 |
  bes8.\noBeam( bes16\noBeam) bes8 bes4 d8\rest |
  
  f4 f8\noBeam f8.\noBeam f16\noBeam f8 |
  bes,4.~ bes4 d8\rest |
  f,4 f8\noBeam f8.\noBeam f16\noBeam f8 |
  bes4.~ bes4 d8\rest \bar "|."
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
    \new Lyrics \with { alignAboveContext = #"women" } \lyricsto "sopranos" \sopWords
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
    \tempo 8 = 112
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
