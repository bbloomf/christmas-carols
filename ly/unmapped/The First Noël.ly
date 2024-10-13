\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"The First Noël"}}
  poet = \markup\oldStyleNum"Traditional"
  composer = \markup\oldStyleNum"18th Century French Melody"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
}
\paper {
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #016
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
  \time 3/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-0.5 . 0.5)
}

sopMusic = \relative c' {
  \partial 4 fis8[^\mf e] |
  d4.( e8) fis[ g] |
  a2 b8[ cis] |
  \slurDotted d4( cis) b |
  a2 b8\noBeam( cis) |
  d4( cis) b |
  
  a( b) cis |
  d( a) g |
  fis2 \slurSolid fis8[ e] |
  d4.( e8) \slurDotted fis8\noBeam( g) |
  \slurSolid a2 b8[ cis] |
  
  \slurDotted d4( cis) b |
  a2 b8\noBeam( cis) |
  d4( cis) b |
  a( b) cis |
  \slurSolid d( a) g |
  \partial 2 fis2 | \break
  
  \partial 4 fis8[ e] |
  d4.( e8) fis[ g] |
  a2 d8[ cis] |
  b2 b4 |
  a2. |
  d4 cis b |
  a( b) cis |
  d( a) g |
  \partial 2 fis2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d4 |
  a2 d4 |
  e2 e4 |
  \slurDotted a4( a) g |
  fis2 d8\noBeam( e) |
  a4( fis) g |
  
  a( d,) g |
  fis( fis) e |
  d2 cis4 |
  a2 d8\noBeam( d) |
  e2 e4 |
  
  a4( a) g |
  fis2 d8\noBeam( e) |
  a4( fis) g |
  a( g) g |
  fis2 e4 |
  d2 |
  
  cis4 |
  a2 d4 |
  cis2 fis4 |
  g2 g4 |
  \slurSolid fis2( a4) |
  fis4 fis g |
  fis2 g4 |
  fis2 e4 |
  d2 \bar "|."
}
altoWords = {
  \dropLyricsVI
  \lyricmode {
    \set stanza = #"1. "
    The first __ No -- ël the
    \set ignoreMelismata = ##t
    an -- gel did say,
    Was to cer -- tain poor shep -- herds in fields as they lay;
    \unset ignoreMelismata
    
    In __ fields __ where they lay __
    \set ignoreMelismata = ##t
    
    keep -- ing their sheep
    On a cold win -- ter’s night _ that was __ _ so deep.
    \unset ignoreMelismata
  }
  \set stanza = \markup\dynamic"ff   "
  \lyricmode {
    \dropLyricsIV
    No -- ël, __ No -- ël, No -- ël,
    \set associatedVoice = "altos"
    No -- ël, __
    \unset associatedVoice
    Born is the King of Is -- ra -- el.
  }
}
altoWordsII = \lyricmode {
  \dropLyricsVI
  \set stanza = #"2. "
  They look -- ed __ up and
  \set ignoreMelismata = ##t
  saw __ _ a Star
  Shin -- ing in __ _ the East __ _ be -- yond __ _ them far,
  \unset ignoreMelismata
  
  And to __ the __ earth it __
  \set ignoreMelismata = ##t
  
  gave __ _ great light,
  And _ so it con -- tin -- ued both day __ _ and night.
}
altoWordsIII = \lyricmode {
  \dropLyricsVI
  \set stanza = #"3. "
  And by
%8.5x11 __
  the __ light of __ that __ same Star
  Three wise men came __ from coun -- try far;
  To __ seek __
\set ignoreMelismata = ##t
  for a King
\unset ignoreMelismata
  was their
%8.5x11 __
  in -- tent,
\set ignoreMelismata = ##t
  And to fol -- low the star _ where e’er __ _ it went.
}
altoWordsIV = \lyricmode {
  \dropLyricsVI
  \set stanza = #"4. "
  This star __ drew nigh to __ 
  \set ignoreMelismata = ##t
  the __ _ North West,
  O’er
%8.5x11 __
  _ Beth -- _ le -- hem __ _ it took __ _ its rest,
  \unset ignoreMelismata
  
  And there __ it __ did both
  \set ignoreMelismata = ##t
  
  stop __ _ and stay
  Right _ o -- ver the place _ where Je -- _ sus lay.
}
altoWordsV = \lyricmode {
  \dropLyricsVI
  \set stanza = #"5. "
  Then en -- ter’d in those
  \set ignoreMelismata = ##t
  Wise -- _ men three,
  Full __ _ rev -- _ ’rent -- ly __ _ on bend -- _ ed knee,
  \unset ignoreMelismata
  
  And of -- fer’d
%8.5x11 __
  there in __
  \set ignoreMelismata = ##t
  
  His __ _ pres -- ence,
  Their _ gold __ _ and myrrh _ and frank -- _ in -- cense.
}
altoWordsVI = \lyricmode {
  \dropLyricsVI
  \set stanza = #"6. "
  Then let __ us __ all with one __ ac -- cord,
  Sing prais -- es to __ our
\set ignoreMelismata = ##t
  Heav -- en -- ly Lord,
\unset ignoreMelismata
  That hath __ made Heav’n and earth of naught,
  And with __ His Blood man -- kind __ hath bought.
}

tenorMusic = \relative c' {
  a8[ g] |
  fis2 d'4 |
  cis2 b4 |
  \slurDotted a4( a) \slurSolid b8[ cis] |
  d2 \slurDotted b8\noBeam( a) |
  a4( a) cis |
  
  d( b) g |
  a( d) a |
  a2 \slurSolid a8[ g] |
  fis2 \slurDotted d'8\noBeam( d) |
  \slurSolid d4( cis) b |
  
  \slurDotted a( a) \slurSolid b8[ cis] |
  d2 \slurDotted b8\noBeam( a) |
  a4( a) cis |
  d4( d) e |
  \slurSolid a,( d) a |
  a2 |
  
  a8[ g] |
  fis2 b4 |
  a2 a4 |
  b4.( cis8) d[ e] |
  fis2( e4) |
  d d d |
  d2 g,4 |
  a2 a4 |
  a2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  d4 |
  d2 b4 |
  a( a') g |
  \slurDotted fis( fis) g |
  d2 g8\noBeam( g) |
  fis4( d) e |
  
  fis( g) e |
  fis8[( g] a4) a |
  d,2 a4 |
  d2 b8\noBeam( b) |
  \slurSolid a4( a') g |
  
  \slurDotted fis( fis) g |
  d2 g8\noBeam( g) |
  fis4( d) e |
  fis( g) e |
  \slurSolid fis8[( g] a4) a, |
  d2 |
  
  a4 |
  d2 b4 |
  fis'2 d4 |
  g4.( a8) b[ cis] |
  d2( cis4) |
  b a g |
  d'( d,) e |
  fis8[( g] a4) a, |
  d2 \bar "|."
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
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWords
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
