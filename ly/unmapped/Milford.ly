\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Milford"}}
  poet = \markup\oldStyleNum"Jeremiah Clarke (1674–1707)"
  composer = \markup\oldStyleNum"Joseph Stephenson (1728–1810)"
  tagline = \markup \concat { "from " \italic "The American Vocalist: a selection of tunes, anthems, sentences, and hymns, old and new" \oldStyleNum", 1849, via " \italic "hymnary.org"}
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
       (padding . -5)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #191
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
  \key a \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c'' {
  b1\rest |
  b1\rest |
  e2 cis4 cis |
  a4.( b8[ a b]) cis[ d] |
  e4 e b b |
  cis cis e e |
  cis4.( b8 a4) gis |
  a2. gis4 |
  a2 cis |
  b2. a4 |
  \partial 2. b2.
  \repeat volta 2 {
    \partial 4 b4\rest |
    b1\rest |
    
    b2.\rest d4\rest |
    d2.\rest a4 |
    cis4 cis e e |
    b2. a4 |
    gis2. r4 |
    r2. a4 |
    e'4 e cis cis |
    a2. a4 |
    cis cis e e |
    cis cis e fis |
    e2. d4 |
  }
  \alternative {
    {
      \partial 2. cis2.
    }
    {
      cis1 \bar "|."
    }
  }
}
sopWords = \lyricmode {
  If an -- gels sung __ a Sav -- ior’s birth,
  If an -- gels sung a Sav -- ior’s birth,
  On that au -- spi -- cious morn,
  
  We well may im -- i -- tate their mirth,
  Now He a -- gain is born,
  Now He a -- gain is born,
  Now He a -- gain is born.
  
  born.
}

altoMusic = \relative c' {
  s1 |
  s1 |
  r1 |
  a'2 fis4 fis |
  e4.( fis8[ e fis]) gis4 |
  a2. gis4 |
  fis2( e4) e |
  e2. e4 |
  e2 <fis a> |
  fis2. fis4 |
  fis2. |
  \repeat volta 2 {
    s4 |
    s1 |
    
    s2. cis4 |
    e e a a |
    a1( |
    gis2.) fis4 |
    e2. e4 |
    a a fis fis |
    e1~|
    e2 r2 |
    e2 gis4 a |
    e e a a |
    gis2. gis4 |
  }
  \alternative {
    {
      a2.
    }
    {
      a1
    }
  }
}
altoWords = \lyricmode {
  If an -- gels sung __ a Sav -- ior’s, Sav -- ior’s birth,
  On that au -- spi -- cious morn,
  
  We well may im -- i -- tate __ their mirth,
  Now He a -- gain is born, __
  Now He a -- gain,
  Now He a -- gain is born.
  
  born.
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
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
  e2 cis4 cis |
  a4.( b8[ a b]) cis[ d] |
  e2. e,4 |
  a2. a4 |
  cis cis e e |
  cis4.( b8 a4) e |
  a4.( b8 cis4) b |
  a2. b4 |
  cis2 a |
  d cis |
  b2.
  \repeat volta 2 {
    r4 |
    r2. a4 |
    cis cis e e |
    cis cis a a |
    e' e cis cis |
    fis2( e4) dis |
    e2. r4 |
    r1 |
    r1 |
    r2. a,4 |
    e' e cis cis |
    a a cis d |
    b2. b8[ a] |
  }
  \alternative {
    {
      a2.
    }
    {
      a1
    }
  }
}
tenorWords = \lyricmode {
  If an -- gels sung __ a Sav -- ior’s birth,
  If an -- gels sung a Sav -- ior’s, Sav -- ior’s birth,
  On that au -- spi -- cious morn,
  
  We well may im -- i -- tate their mirth,
  We well may im -- i -- tate __ their mirth,
  Now He a -- gain is born,
  Now He a -- gain is born.
  
  born.
}

bassMusic = \relative c' {
  r1 |
  a2 fis4 fis |
  e4.( fis8[ e fis]) gis4 |
  a2. a,4 |
  e'2. e4 |
  a a cis cis |
  a( d, e) e |
  a,2. e'4 |
  a2 fis |
  d fis |
  b,2. |
  \repeat volta 2 {
    b4 |
    e e a a |
    
    fis fis e e |
    a a cis cis |
    a1( |
    b2.) b,4 |
    e2. r4 |
    r1 |
    r2. e4 |
    a a fis fis |
    e2. a4 |
    <a cis> <a cis> a( d, |
    e2.) e4 |
  }
  \alternative {
    {
      a,2.
    }
    {
      a1
    }
  }
}
bassWords = \lyricmode {
  If an -- gels sung __ a Sav -- ior’s birth,
  If an -- gels sung a Sav -- ior’s birth,
  On that au -- spi -- cious morn,
  
  We well may im -- i -- tate their mirth,
  We well may im -- i -- tate __ their mirth,
  Now He a -- gain is born,
  Now He a -- gain __ is born.
  
  born.
}

pianoRH = \relative c' {
  
}
pianoLH = \relative c' {
  
}

\score {
  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { << \global \voiceOne \sopMusic >> }
      \new Voice = "altos" { << \global \voiceTwo \altoMusic >> }
    >>
    \new Staff = tenors <<
      \clef "G_8"
      \new Voice = "tenors" { << \global \tenorMusic >> }
    >>
    \new Staff = basses <<
      \clef bass
      \new Voice = "basses" { << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWords
   
    \new Lyrics \with { alignBelowContext = #"tenors" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"basses" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
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
      \new Voice = "sopranos" { << \global \voiceOne \sopMusic >> }
      \new Voice = "altos" { << \global \voiceTwo \altoMusic >> }
    >>
    \new Staff = tenors <<
      \clef "G_8"
      \new Voice = "tenors" { << \global \tenorMusic >> }
    >>
    \new Staff = basses <<
      \clef bass
      \new Voice = "basses" { << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWords
   
    \new Lyrics \with { alignBelowContext = #"tenors" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"basses" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \midi {
    \tempo 4 = 180
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}

