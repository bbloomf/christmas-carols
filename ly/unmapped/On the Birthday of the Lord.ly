\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"On the Birthday of the Lord"}}
  poet = \markup\concat{\italic"In natali Domini" \oldStyleNum", 14th Century"}
  meter = \markup\oldStyleNum"Translated by Rev. H. R. Bramley (1833–1917) from Latin"
  composer = \markup\oldStyleNum"John Bacchus Dykes (1823–1876)"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
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
  first-page-number = #045
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
  \key d \major
  \time 4/4
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(1 . 1)
  \override DynamicText #'X-offset = #-4
}

sopMusic = \relative c' {
  d2_\f fis4( g) |
  a2 a |
  b4( cis) d2 |
  d1 \bar "||"
  a2_\p b |
  b4( a) g( fis) |
  
  g2 a |
  fis1 \bar "||"
  d2_\f fis4( g) |
  a2 a |
  b4( cis) d2 |
  d1 \bar "||"
  
  a2_\ff b |
  cis2. cis4 |
  cis2 cis |
  cis1 |
  cis2 d |
  e d4( cis) |
  b2 b |
  a1 \bar "||"
  \time 3/2 \break
  a1^\markup\italic"dim." b2 |
  a1 fis2 |
  a2 b1 |
  a1. \bar "||"
  
  d1 d2 |
  cis1 b2 |
  a1 b2 |
  a1. \bar "||"
  a1^\markup\italic"dim." a2 |
  
  d1^\< d2\! |
  d1.~ |
  d2( cis b) |
  a1_\pp fis2 e1 a2 |
  d,1. \bar "|."
}
sopWords = \lyricmode {
  \repeat unfold 42 { \skip 1 }
%  Ma -- ry
}

altoMusic = \relative c' {
  d2 fis4( g) |
  a2 a |
  b4( cis) d2 |
  d1 \bar "||"
  d,2 b4( cis) |
  d2 d |
  
  d cis? |
  d1 \bar "||"
  d2 fis4( g) |
  a2 fis4( e) |
  d2 d |
  d1 \bar "||"
  
  d2 fis |
  fis2. fis4 |
  fis2 eis |
  fis1 |
  a2 gis4( fis) |
  e( a) b( a) |
  
  a2 gis |
  a1 \bar "||"
  \time 3/2
  fis1 fis2 |
  fis1 d2 |
  fis2 fis1 |
  fis1( a2) \bar "||"
  
  a2( gis4 fis) gis2 |
  a1 fis2 |
  e1 d2 |
  cis1. \bar "||"
  d1 e2 |
  
  fis1 fis2 |
  g1.~ g1. |
  fis1 d2 |
  cis1 cis2 |
  d1. \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  On the Birth -- day of __ \set associatedVoice = "sopranos" the Lord \unset associatedVoice
  An -- gels joy in glad ac -- cord,
  And they sing in sweet -- est tone
  Glo -- ry be to God a -- lone,
  
  Glo -- ry be \set associatedVoice = "sopranos" to __ God a -- lone. \unset associatedVoice
  
  God is born of maid -- en fair, __
  Ma -- ry doth the Sav -- ior bear;
  \set associatedVoice = "sopranos"
  Ma -- ry ev -- er pure, __
  Ma -- ry ev -- er pure.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  These good news an An -- \set associatedVoice = "sopranos" gel told \unset associatedVoice
  To the shep -- herds by their fold,
  Told them of the Sav -- ior’s Birth,
  Told them of the joy for earth,
  Told them of \set associatedVoice = "sopranos" the __ joy for earth.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  Born is __ now Em -- man -- \set associatedVoice = "sopranos" u -- el, \unset associatedVoice
  He, an -- nounced by Ga -- bri -- el,
  He, Whom Pro -- phets old at -- test,
  Com -- eth from His Fa -- ther’s Breast,
  Com -- eth from \set associatedVoice = "sopranos" His __ Fa -- ther’s Breast.
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  Born to -- day is Christ \set associatedVoice = "sopranos" the Child, \unset associatedVoice
  Born of Ma -- ry un -- de -- filed,
  Born the __ King and Lord we own;
  Glo -- ry be to God a -- lone,
  Glo -- ry be \set associatedVoice = "sopranos" to __ God a -- lone.
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
  d2^\f fis4( g) |
  a2 a |
  b4( cis) d2 |
  d1 \bar "||"
  
  d2^\p g, |
  a d |
  
  c4( b) a( g) |
  a1 \bar "||"
  d,2^\f fis4( g) |
  a2 d |
  d cis4( b) |
  a1 \bar "||"
  
  a4(^\ff g) fis( gis) |
  a2. a4 |
  gis2 gis |
  a1 |
  a2 a |
  e' fis |
  
  b,4( cis) d( e) |
  cis1 \bar "||"
  \time 3/2
  d1 d2 |
  d1 a2 |
  d2 d1 |
  d1( e2) \bar "||"
  
  d1 d2 |
  e1 d2 |
  cis1 gis2 |
  a1( g!2) \bar "||"
  fis1 g2 |
  
  a1 ais2 |
  b1.~ |
  b2( cis d) |
  d1^\pp a2 |
  g1 g2 |
  fis1. \bar "|."
}
tenorWords = \lyricmode {
  \repeat unfold 42 { \skip 1 }
  %Ma -- ry
}

bassMusic = \relative c {
  d2 fis4( g) |
  a2 a |
  b4( cis) d2 |
  d1 \bar "||"
  fis,2 g |
  fis fis |
  
  e e |
  d1 \bar "||"
  d2 fis4( g) |
  a2 d4( cis) |
  b2 a4( g) |
  fis1 \bar "||"
  
  fis4( e) d2 |
  cis2. cis4 |
  cis2 cis |
  fis1 |
  fis2 e4( d) |
  cis2 d |
  
  e e |
  a1 \bar "||"
  \time 3/2
  d,1 b2 |
  d1 d2 |
  d'2 b1 |
  d1( cis2) \bar "||"
  
  b1 b2 |
  a2( cis,) d |
  e1 e2 |
  a,1. \bar "||"
  d1 d2 |
  
  d1 d2 |
  g,1.~ |
  g |
  a1 a2 |
  a1 a2 |
  d1. \bar "|."
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
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 0) (padding . -1))} \lyricsto "sopranos" \sopWords
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 0))} \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 0))} \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 0))} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 0))} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 0))} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 0) (padding . -1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 0)) } \lyricsto "basses" \bassWords
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
    \tempo 4 = 180
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
