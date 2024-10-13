\version "2.14.2"
\include "util.ly"
\header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Tollite Hostias"}}
    composer = \markup\oldStyleNum"Camille Saint-Saëns (1835–1921)"
    tagline = \markup { "from" \italic "cpdl.org"}
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
       (padding . 1)
       (stretchability . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #192
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

\markup \fill-line {\concat{ "from " \italic "cpdl.org"}}
\markup\vspace#2












global = {
  \key g \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c'' {
  \tempo "Maestoso"
  \repeat volta 2 {
    d2 d4 d |
    g4. d8 d2 |
    e2 d4 c |
    b2 a |
    c4 c b4. b8 |
    e4. e8 d2 |
    
    c4 d b( c) a2. b4\rest \break
  }
  \repeat volta 2 {
    a2^\p a4 b |
    g2 g4 a |
    b2 b4 c |
    a2 b |
    
    %page2
    b4 a8 g d'4 d8 d |
    d4 e8 fis g4 fis8[ e] |
    d4 c8[ b] a4 a
  }
  \alternative {
    {
      g2. b4\rest \break
    }
    {
      g2. b4\rest
    }
  }
  b4\rest g g fis |
  
  e e' e d |
  c c c b |
  a1~ |
  a4 g g a |
  b b cis d |
  e( fis g2)~ |
  
  %page3
  g4 g fis e |
  d d d cis |
  d2 b\rest |
  
  
  a2^\ff a4 b |
  g2 g4 a |
  b2 b4 c |
  a2 b |
  
  b4 a8 g d'4 d8 d |
  d4 e8 fis g4 fis8[ e] |
  d4 c8[ b] a4 a |
  g1 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  \repeat volta 2 {
    d2 g4 fis |
    g4. b8 b2 |
    b a4 a |
    g2 fis |
    g4 a g4. g8 |
    g4. a8 d,2 |
    
    a'4 a g( a) |
    fis2. s4 |
  }
  
  
  \repeat volta 2 {
    fis2 fis4 fis |
    e2 e4 d |
    d2 g4 g |
    fis2 g |
    
    %page2
    g4 fis8 e d4 g8 a |
    b4 c8 d e4 d8[ c] |
    b4 g g fis |
  }
  \alternative {
    { d2. s4 }
    { d2. s4 }
  }
  s4 e e d |
  
  c c' c b |
  a a a g |
  fis fis fis e |
  d d d c |
  b d e fis |
  g( a b2)~ |
  
  %page3
  b4 b a g |
  fis b a a |
  a2 s |
  
  fis2 fis4 fis |
  e2 e4 d |
  d2 g4 g |
  fis2 g |
  
  %page2
  g4 fis8 e d4 g8 a |
  b4 c8 d e4 d8[ c] |
  b4 g g fis |
  d1 \bar "|."
}
altoWords = {
  \dropLyricsVIII
  \set stanza = \markup\dynamic"f   "
  \lyricmode {
    Tol -- li -- te hos -- ti -- as.
    et ad -- o -- ra -- te
    Do -- mi -- num in a -- tri -- o
    sanc -- to e -- jus.
  }
  \raiseLyrics
  \lyricmode {
    Læ -- ten -- tur cæ -- li, et ex -- ul -- tet ter -- ra
    
    a fa -- ci -- e Do -- mi -- ni, quo -- ni -- am ve -- nit. \set associatedVoice = "sopranos" Al -- le -- lu -- ia.
    \unset associatedVoice
    ia.
    
    
    Al -- le -- lu -- ia.
    Al -- le -- lu -- ia.
    Al -- le -- lu -- ia.
    Al -- le -- lu -- ia.
    Al -- le -- lu -- ia.
    Al -- le -- lu -- ia. __
    Al -- le -- lu -- ia.
    Al -- le -- lu -- ia.
    
    \set associatedVoice = "tenors"
    Læ -- ten -- tur cæ -- li, et ex -- ul -- tet ter -- ra
    \unset associatedVoice
    a fa -- ci -- e Do -- mi -- ni, quo -- ni -- am ve -- nit. \set associatedVoice = "sopranos" Al -- le -- lu -- ia.
  }
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
  \repeat volta 2 {
    b2 b4 c |
    d4. d8 d2 |
    g2 d4 d |
    d2 d |
    g,4 d' d4. d8 |
    e4. fis8 g2 |
    
    e4 d d( e) |
    d2. s4 |
  }
  
  
  \repeat volta 2 {
    a2 d4 d |
    b2 b4 a |
    g2 b4 e |
    d2 d |
    
    %page2
    b4 c d c |
    b a g a |
    b e d d |
  }
  \alternative {
    { b2. s4 }
    { b4 b b a }
  }
  g1~ |
  
  g~ |
  g4 fis g b |
  d d, d c |
  b b' b a |
  g b b a |
  g g' g fis |
  
  %page3
  e a, b cis |
  a d fis e |
  d2 s |
  
  
  a2 d4 d |
  b2 b4 a |
  g2 b4 e |
  d2 d |
  
  b4 c d c |
  b a g a |
  b e d d |
  b1 \bar "|."
}
tenorWords = \lyricmode {
  \repeat unfold 22 \skip1
  Læ -- ten -- tur cæ -- li, et ex -- ul -- tet ter -- ra
  quo -- ni -- am ve -- nit.
  Al -- le -- lu -- ia.
  Al -- le -- lu -- ia.
  ia.
  
  Al -- le -- lu -- ia. __
  Al -- le -- lu -- ia.
  Al -- le -- lu -- ia.
  Al -- le -- lu -- ia.
  Al -- le -- lu -- ia.
  Al -- le -- lu -- ia.
  Al -- le -- lu -- ia.
  Al -- le -- lu -- ia.
  
  Læ -- ten -- tur cæ -- li, et ex -- ul -- tet ter -- ra
  quo -- ni -- am ve -- nit.
  Al -- le -- lu -- ia.
  Al -- le -- lu -- ia.
}

bassMusic = \relative c {
  \repeat volta 2 {
    g'2 g4 a |
    b4. g8 g2 |
    e fis4 d |
    g2 d |
    e4 fis g4. g8 |
    c4. c8 b2 |
    
    a4 fis g( c,) |
    d2. d4\rest |
  }
  
  \repeat volta 2 {
    d2 d4 b |
    e2 e4 fis |
    g2 e4 c |
    d2 g |
    
    g4 a b a |
    g fis e fis |
    g c, d d |
  }
  \alternative {
    { g,2. d'4\rest }
    { g,2. r4 }
  }
  r1 |
  
  r |
  r |
  r4 d' d c |
  b b' b a |
  g g g fis |
  e <e e'> q <d d'> |
  
  <cis cis'> cis d e |
  fis g a a, |
  d2 d\rest |
  
  
  d2 d4 b |
  e2 e4 fis |
  g4( fis) e4 c |
  d2 g |
  
  g4 a b a |
  g fis e fis |
  g c, d d |
  g,1 \bar "|."
}
bassWords = \lyricmode {

}

pianoRH = \relative c' {
  \set Staff.midiInstrument = "acoustic grand"
  \repeat volta 2 {
    <g' b d>2_\f q4 <fis c' d> |
    <g d' g>4. <g b d>8 q2 |
    <g b e> <d a' d>4 <d a' c> |
    <d g b>2 <d fis a> |
    <g c>4 <d a' c> <d g b>4. q8 |
    <e g e'>4. <fis a e'>8 <g b d>2 |
    
    <e a c>4 <d a' d> <d g b> <e a c> |
    <d fis a>2. r4 
  }
  \repeat volta 2 {
    <a fis' a>2_\p <d fis a>4 <d fis b> |
    <b e g>2 q4 <a d a'> |
    <d b'>2 <b g' b>4 <e g c> |
    <d fis a>2 <d g b> |
    
    %page2
    <b g' b>4 << {<fis' a>8[ <e g>] d'4 <d g,>8[ <d a>] } \\
                 {c,4 d c} >> |
    << {d'4 <c e>8[ <d fis>] <e g>4 <d fis>8[ <c e>] |
        d4 c8[ b] <a g>4 <a fis>} \\
       {b4 a g a |
        b <g e> d d} >>
  }
  \alternative {
    {<b d g>2. r4}
    {<b d g>4 b b a}
  }
  r4 <e' g> q <d fis> |
  
  <c e> <c' e> q <b d> |
  <a c> q q <g b> |
  << { a1~ | a4 } \\
     { fis4 fis fis e | d } >>
  <d g> q <c a'> |
  <b b'> <d b'> <e cis'> <fis d'> |
  <g e'> <a fis'> <b g'>2~ |
  
  %page3
  q4 q <a fis'> <g e'> |
  <fis d'> <b d> <fis a d> <e a cis> |
  <a d>4 \change Staff = "two" \stemUp d,,8[_\( fis a]
         \change Staff = "one" d[ fis g]\) |
  \stemNeutral
  <d fis a>2_\ff q4 <d fis b> |
  <b e g>2 q4 <a d a'> |
  
  <d g b>2 <e g b>4 <e g c> |
  <d fis a>2 <d g b> |
  <b g' b>4 << {<fis' a>8[ <e g>] d'4 <d g,>8[ <d a>] } \\
                 {c,4 d c} >> |
    << {d'4 <c e>8[ <d fis>] <e g>4 <d fis>8[ <c e>] |
        d4 c8[ b] <a g>4 <a fis>} \\
       {b4 a g a |
        b <g e> d d} >> |
  <b d g>1 \bar "|."
  
}
pianoLH = \relative c' {
  \set Staff.midiInstrument = "acoustic grand"
  \repeat volta 2 {
    <g g,>2 q4 <a a,> |
    <b b,>4. <g g,>8 q2 |
    <e e,> <fis fis,>4 <d d,> |
    <g g,>2 <d d,> |
    <e e,>4 <fis fis,> <g g,>4. q8 |
    <c c,>4. q8 <b b,>2 |
    
    <a a,>4 <fis fis,> <g g,> <c, c,> |
    <d d,>2. r4 
  }
  \repeat volta 2 {
    <d d,>2 q4 <b b,> |
    <e e,>2 q4 <fis fis,> |
    <g g,>2 <e e,>4 <c c,> |
    <d d,>2 <g g,> |
    
    %page 2
    <g g,>4 <a a,> <b b,> <a a,> |
    <g g,> <fis fis,> <e e,> <fis fis,> |
    <g g,> <c, c,> <d d,> q |
  }
  \alternative {
    {
      <g, g,>2. r4
    }
    {
      <g g,>2. r4 |
    }
  }
  g'1~ |
  
  g~ |
  g4 fis g b |
  d d, d c |
  b b' b a |
  g <g b> q <fis a> |
  <e g> <e' g> q <d fis> |
  
  %page 3
  <cis e>4 <a cis,> <b d,> <cis e,> |
  <a fis> <g d'> a a, |
  <d d'> s2. |
  <d d,>2 q4 <b b,> |
  <e e,>2 q4 <fis fis,> |
  
  <g g,> <fis fis,> <e e,> <c c,> |
  <d d,>2 <g g,> |
  q4 <a a,> <b b,> <a a,> |
  <g g,> <fis fis,> <e e,> <fis fis,> |
  <g g,> <c, c,> <d d,> q |
  <g, g,>1 \bar "|."
}

\score {
  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWords
   \new Lyrics \with { alignAboveContext = #"men" } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
  \new PianoStaff <<
\new Staff = "one" { \new Voice { \global \pianoRH } } \new Staff = "two" { \global \clef "bass" \pianoLH } >>
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
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
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
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWords
   \new Lyrics \with { alignAboveContext = #"men" } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
  \new PianoStaff <<
\new Staff = "one" { \new Voice { \global \pianoRH } } \new Staff = "two" { \global \clef "bass" \pianoLH } >>
>>
  
  
  \midi {
    \tempo 4 = 105
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}

