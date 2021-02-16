﻿\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smbd Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Noël Nouvelet"}}
  poet = \markup\column {\concat{"15th Century French Carol from "\italic "Le Grande Bible des Noels"} "Translated by P.S.B."}
  composer = \markup\oldStyleNum"15th Century French Carol"
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
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
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #175
  print-first-page-number = ##t
  headerLine = \markup{ \smallCapsOldStyle"christmas"}
  oddHeaderMarkup = \markup\fill-line{
     \abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premr Pro" "Garamond Premr Pro" "Garamond Premr Pro" (/ 18 20))) }
global = {
  \key bes \major
  \time 2/4
  \autoBeamOff
}

sopMusic = \relative c'' {
  g8 d' e c |
  d4 bes |
  c8 c16[ d] bes8 a |
  g2 | \break
  
  \slurDashed
  g8( d') e c |
  \slurSolid
  d4 bes |
  c8 c16[ d] bes8 a |
  g2 | \break
  
  bes4 a8 g |
  a4. d,8 |
  bes' bes a g |
  a2 | \break
  
  g8 d' e c |
  d4 bes |
  c8 c16[ d] bes8 a |
  g2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  g'8 d g g |
  d4 g |
  e8 e g fis |
  d2 |
  
  \slurDashed
  g8( d) g g |
  \slurSolid
  d4 g |
  e8 e g fis |
  d2 |
  
  d4 e8 e |
  d4. d8 |
  d d e e |
  d2 |
  
  g8 d g g |
  d4 g |
  e8 e g fis |
  d2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  \markup\italic “No -- \markup\italic ël \markup\italic nou -- \markup\italic ve -- \markup\italic let,”
  come let us sing \markup\italic “no -- \markup\italic ël;”
  \set ignoreMelismata = ##t
  Let us faith -- ful folk,
  \unset ignoreMelismata
  cry out our thanks to God!
  
  Sing we \markup\italic “no -- \markup\italic ël,”
  un -- to the ti -- ny King,
  \markup\italic “No -- \markup\italic ël \markup\italic nou -- \markup\italic ve -- \markup\italic let,”
  come let us sing \markup\italic “no -- \markup\italic ël.”
}
altoWordsII = \lyricmode {
  \dropLyricsV
  \set stanza = #"2. "
  Prais -- es to our Lord, our Sav -- ior Je -- sus Christ,
  \set ignoreMelismata = ##t
  Come to earth as man, 
  \unset ignoreMelismata
  as man to live and die,
  
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \markup\italic No -- \markup\italic ël \markup\italic nou -- \markup\italic ve -- \markup\italic let, \markup\italic No -- \markup\italic ël \markup\italic chan -- \markup\italic tons \markup\italic i -- \markup\italic ci,
  \markup\italic Dé -- \markup\italic vo -- \markup\italic tes \markup\italic gens, \markup\italic cri -- \markup\italic ons \markup\italic à \markup\italic Dieu \markup\italic mer -- \markup\italic ci!
  
  \markup\italic Chan -- \markup\italic tons \markup\italic No -- \markup\italic ël \markup\italic pour \markup\italic le \markup\italic Roi \markup\italic nou -- \markup\italic ve -- \markup\italic let!
  \markup\italic No -- \markup\italic ël \markup\italic nou -- \markup\italic ve -- \markup\italic let, \markup\italic No -- \markup\italic ël \markup\italic chan -- \markup\italic tons \markup\italic i -- \markup\italic ci!
}
altoWordsIV = \lyricmode {
}

tenorMusic = \relative c' {
  g8 bes c c |
  bes4 d |
  c8 g d' d |
  bes2 |
  
  \slurDashed
  g8( bes) c c |
  \slurSolid
  bes4 d |
  c8 g d' d |
  bes2 |
  
  bes4 c8 c |
  a[ g fis] d |
  bes' bes c c |
  fis,([ g] a4) |
  
  g8 bes c c |
  bes4 d |
  c8 g d' c |
  bes2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c' {
  g8 g c, e |
  g4 g |
  c,8 c d d |
  g2 |
  
  \slurDashed
  g8( g) c,8 e |
  \slurSolid
  g4 g |
  c,8 c d d |
  g2 |
  
  g4 a8 a |
  fis8[ e d] d8 |
  g g a a |
  d,([ e] fis4) |
  
  g8 g c, e |
  g4 g |
  c,8 c d d |
  g,2 \bar "|."
}
bassWords = \lyricmode {

}

\score {
  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "sopranos" \altoWords
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
    \Lyrics
    \override LyricText #'font-size = #1.3
  }
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

