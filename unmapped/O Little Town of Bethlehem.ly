﻿\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"O Little Town of Bethlehem"}}
  poet = \markup\oldStyleNum"Phillips Brooks (1835–1893)"
  composer = \markup\oldStyleNum"Lewis H. Redner (1831–1908)"
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
  first-page-number = #039
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"christmas"}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 15) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 15 20))) }
global = {
  \key g \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c'' {
  \partial 4 b4 |
  b b ais b |
  \slurDotted d( c) e, a |
  \slurSolid g fis8[ g] a4 d, |
  b'2. \bar""\break b4 |
  
  b b e d |
  d c e, a |
  g fis8[ g] b4. a8 |
  g2. \bar""\break b4 |
  
  b b a g |
  fis2 fis4 fis |
  e fis g a |
  b2. \bar""\break b4 |
  
  b b ais b |
  d c e, e' |
  d g, b4. a8 |
  \partial 4*3 g2. \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d4 |
  d d cis d |
  \slurDotted f( e) c e |
  d d d d |
  d2. d4 |
  
  d g gis gis |
  a e c e |
  d d fis fis |
  g2. g4 |
  
  g g fis e |
  dis2 dis4 dis |
  e fis g e |
  fis2. g4 |
  
  d d cis d |
  \slurSolid e e c e8[ fis] |
  g4 cis, d4 c |
  b2. \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  O lit -- tle town of Beth -- le -- hem,
  How still we _ see thee lie!
  A -- bove thy deep and dream -- less sleep
  The si -- lent _ \set associatedVoice = "altos" stars go by; \unset associatedVoice
  Yet in thy dark streets shin -- eth
  The ev -- er -- last -- ing Light;
  The hopes and fears of all the years
  Are met in \set associatedVoice = "altos" thee to -- night.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  For Christ is born of Ma -- _ ry,
  And gath -- er’d _ all a -- bove,
  While mor -- tals sleep, the an -- gels keep
  Their watch of _ \set associatedVoice = "altos" won -- d’ring love. \unset associatedVoice
  O morn -- ning stars, to -- geth -- er
  Pro -- claim the ho -- ly birth!
  And prais -- es sing to God the King,
  And peace to \set associatedVoice = "altos" men on earth!
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  How si -- lent -- ly, how si -- lent -- ly
  The won -- drous _ gift is giv’n!
  So God im -- parts to hu -- man hearts
  The bless -- ings _ \set associatedVoice = "altos" of His Heav’n. \unset associatedVoice
  No ear may hear His com -- ing,
  But in this world of sin;
  Where meek souls will re -- ceive Him still,
  The dear Christ \set associatedVoice = "altos" en -- ters in.
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
\set ignoreMelismata = ##t
  \set stanza = #"4. "
  O ho -- ly Child of Beth -- le -- hem!
  De -- scend to _ us, we pray;
  Cast out our sin, and en -- ter in,
  Be born in _ \set associatedVoice = "altos" us to -- day. \unset associatedVoice
  We hear the Christ -- mas an -- gels
  The great glad tid -- ings tell;
  O come to us, a -- bide with us,
  Our Lord \set associatedVoice = "altos" Em -- man -- u -- el!
}

tenorMusic = \relative c' {
  g4 |
  g g g g |
  \slurDotted gis( a) a c |
  \slurSolid b a8[ b] c4 c |
  b2. g4 |
  
  g b b e |
  e e e c |
  b a8[ b] d4 c |
  b2. d4 |
  
  d b c cis |
  dis2 dis4 b |
  e, fis g e' |
  dis2. d4 |
  
  d b g g |
  gis a a a |
  b a8[ g] g4 fis |
  g2. \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c' {
  g4 |
  g g g g |
  \slurDotted c,( c) c c |
  d d d d |
  g,2. g'4 |
  
  g f e e |
  a a a, c |
  d4. d8 d4 d |
  g2. g4 |
  
  g g a ais | 
  b2 b4 b, |
  e fis g c |
  b2. g4 |
  
  g g g g |
  c, c c c |
  d e d4 d |
  g,2. \bar "|."
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
  }
  \midi {
    \tempo 4 = 90
    \set Staff.midiInstrument = "flute"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
