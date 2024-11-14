\version "2.14.2"
\include "util.ly"
\version "2.14.2"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"In the Bleak Midwinter"}}
  poet = \markup\oldStyleNum"Christina Rosetti (1830–1894)"
  composer = \markup\oldStyleNum"Harold Darke (1888–1976)"
  tagline = \markup { "from" \italic {cpdl.org}}
}

\paper {
  %print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 12)
       (minimum-distance . 0)
       (padding . -2)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #180
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
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c'' {
  \oneVoice
  \tempo \markup{\override #'(font-name . "Garamond Premier Pro") "Moderato e tranquillo"}
  r1 |
  r2 r4. \parenthesize b8 |
  b4.^\mp a8 d4 b |
  \slurDotted
  g4.( g8) fis2 |
  e4. fis8 g4 e |
  d2.( d4) |
  
  g4.^\<( g8) b4 g |
  e'2^\mf d4( d) |
  b4 a^\markup\italic"poco rall." d4. g,8 |
  << c1 { s2. s8 \parenthesize b8 }>>|
  b4.^\markup{\dynamic"p"\italic" a tempo"} a8 d4 g, |
  << f2 { s4. \parenthesize e8}>> e2 |
  
  \slurDotted g2(^\> c4)\! e,4 |
  d2.( d4) |
  g4.^\< a8 b4 g |
  g\!( a) b( c) |
  << d1~ {s2. s4^\>}>> |
  d4( d2) b4\! |
  g1 \bar "||" \break
  \slurSolid
  
  %second verse
  \voiceOne
  \partial 4 b4^\f |
  b4. a8 d4 b |
  g2 fis |
  e4.( fis8) g4 e |
  d1 |
  g4.^\< g8 b4 g |
  
  e'4.^\f e8 d2 |
  b4 a d4. g,8 |
  c1 |
  b4. a8 d4 g, |
  f4.( e8) e4 e |
  g4. g8 c4 e,4 |
  
  d2. d4 |
  g4.( a8) b4 g |
  g( a) b( c) |
  d1~^\> |
  d2. b4\! |
  \partial 2. g2.
  \once \override Score.RehearsalMark #'break-visibility = #end-of-line-visible
  \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT \mark "D.C." \bar "||" \break
  
  %Verse 4
  b2^\markup{\dynamic"mp" \italic"semplice"} d4 b |
  g2 fis |
  e2 g4 e |
  d1 |
  g4. g8^\< b4 g\! |
  e'2 d2 |
  b4 a d4. g,8 |
  
  c1 |
  b4. a8 d4 g, |
  f2 e2 |
  g4. g8 c4 e,4 |
  d2. d4^\mf |
  g4. a8 b4 g |
  
  g( a) b( c) |
  d2. d4 |
  g2^>( d) |
  d1~ |
  d2. b4 |
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  s1*19 |
  
  g'4 |
  g4. g8 g4 fis |
  e2 d |
  c c4 a |
  d2( c) |
  b4 d f g |
  
  g a g( fis) |
  g g g4. d8 |
  e2.( d4) |
  d c d d |
  c( d) c c |
  e d c c |
  
  c2 a |
  d4( e8[ fis]) g4 f |
  e( g) g2 |
  g4( fis g a |
  b2.) d,4 |
  d2. \bar "||"
  
  g2 b4 g |
  e2 d |
  c e4 c |
  d2( c) |
  b4 d f g |
  g( a) g( fis) |
  g g g4. d8 |
  
  e2.( d4) |
  d c d d |
  c( d) c2 |
  e4 d c c |
  c2 a |
  d4( e8) fis g4 f |
  
  e( g) g2 |
  g4(^\f^\< fis g) a |
  c(\! b^\> a g) |
  fis2(\> g4 a |
  b2.) d,4 |
  <d g>1^\pp |
}
altoWords = {
  \dropLyricsV
  \set stanza = \markup {\normal-text\italic "Soprano Solo" "1. "}
  \lyricmode {
    _ In the bleak mid -- win -- ter,
    fros -- ty wind made moan, __
    \set ignoreMelismata = ##t
    Earth stood hard as i -- ron, _
    Wa -- ter like a stone. ""
    Snow had fal -- len, snow on snow,
    Snow _ on snow, __ _
    In the bleak mid -- win -- _ ter __ _
    Long __ _ _ a -- go.
    
    \unset ignoreMelismata
    \set stanza = #"2. "
    Our God, Heav’n can -- not hold Him,
    Nor __ earth \set associatedVoice = "tenors"
    sus -- tain; __
    \set associatedVoice = "altos"
    Heav’n and earth shall flee a -- way,
    \set associatedVoice = "sopranos"
    When He comes \set associatedVoice = "altos"
    to reign. __
    
    In the bleak mid -- win -- ter,
    A sta -- ble place suf -- ficed
    the \unset associatedVoice Lord __ God Al -- might -- y __
    Je -- sus Christ.
    
    \set stanza = #"4. "
    \set associatedVoice = "altos"
    What can I give Him,
    Poor as \set associatedVoice = "tenors" I am? __
    If I were a shep -- herd,
    I would bring a lamb; __
    If I were a wise man,
    \set associatedVoice = "altos"
    I would do my part,
    \set associatedVoice = "sopranos"
    Yet "" what I can I give \set associatedVoice = "tenors"
    Him,
    
    Give __ my \set associatedVoice = "basses" heart, __
    give __ \unset associatedVoice my heart.
  }
}
altoWordsII = \lyricmode {
  
}
altoWordsIII = {
  \dropLyricsIV
  \set stanza = \markup {\normal-text\italic "Tenor Solo" "3. "}
  \lyricmode {
    \set ignoreMelismata = ##t
    E -- nough for Him, whom Cher -- u -- bim,
    Wor -- ship night and day,
    A breast __ _ full of milk,
    And a man -- ger full of hay,
    E -- nough for Him, whom an -- _ gels, Fall down be -- fore,
    The ox and ass and ca -- mel, Which a -- dore. __ _
  }
}
altoWordsIV = \lyricmode {} abc = \lyricmode{
\set ignoreMelismata = ##t
  \set stanza = #"4. "
  _ An -- gels and arch -- an -- _ gels May have ga -- thered there _
  Cher -- u -- bim and Ser -- a -- phim _ Throng -- _ ed the air
  But on -- ly His __ _ mo -- _ ther _
  In her maid -- en bliss _
  Wor -- shipped the Be -- lov -- ed with __ _ a __ _ kiss.
}
altoWordsV = \lyricmode {} abc = \lyricmode{
\set ignoreMelismata = ##t
}

tenorMusic = \relative c' {
  s1*19 |
  
  b4 |
  b4. c8 d4 c |
  b2 b |
  g g4 fis |
  g2( fis) |
  g4 b d f |
  
  e fis g( d) |
  d c b4. d8 |
  c2( b4 a) |
  g g g b |
  a( g) g g |
  g4. g8 e4 b' |
  
  a2 fis |
  g4( c) b d |
  c( e) d( c) |
  b( a b c |
  d2.) c4 |
  b2. \bar "||"
  
  %verse 4
  d1 |
  b |
  g |
  g2( fis) |
  g4 b d f |
  e( fis!) g( d) |
  d c b4. d8 |
  
  c2( b4 a) |
  g g g b |
  a( g) g2 |
  g4. g8 e4 b' |
  a2 fis |
  g4^\mf c b d |
  
  c4( e) d( c) |
  b( a b ) c |
  e4^>( d c b) |
  a2( b4 c |
  d2.) c4 |
  b1^\pp
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  s1*19 |
  
  e4 |
  e4. e8 b4 d |
  e2 b |
  c4.( d8) e4 c |
  b2( a) |
  g4. g8 g'4 g |
  
  c4. c8 b4( a) |
  g e b4. b'8 |
  a2( g4 fis) |
  g e b g |
  a( b) c c |
  c b a g |
  
  fis2 c' |
  b4( a) g g' |
  c,( c') b( a) |
  d,1~ |
  d2( e4) fis |
  g2. \bar "||"
  
  %verse 4
  g1( |
  e2 b) |
  c1( |
  b2 a) |
  g4. g8 g'4 g |
  c2 b4( a) |
  g e b4. b'8 |
  
  a2( g4 fis) |
  g e b g |
  a( b) c2 |
  c4 b a g |
  fis2 c' |
  b4 a g g' |
  
  c,( c') b( a) |
  d,2. d4 |
  d1 |
  d~ |
  d2( e4) fis |
  g1
}
bassWords = \lyricmode {

}

pianoRH = \relative c'' {
  \set Staff.midiInstrument = "acoustic grand"
  \set Staff.midiMinimumVolume = #0.5
  \set Staff.midiMaximumVolume = #0.5
  <b d,>4\(_\p <g e> <fis d'> <b fis d> |
  <g b,> <e c> <fis a,> <d c>\) |
  << {<g b,>\( <e c> <fis a,> <d c> |
     g2 <d fis d'>2~ |
     d'4\) c2\( <e fis,>4 | d2 c\) } \\
     {s1 |
      b,4 c s2 | 
      e4. fis8 g4 s | g d e fis } >> |
     
  << {<b d,>4\( <a c,> <d g, d> <b f> | <e g,> <fis a,> <g g,> <d fis,>\)} \\
     {g,2 s | s1} >> |
  << {<b g>4\( <a g> <d g,>4. <g, d>8~ | g4 fis e d\) } \\
     {s1 | c1} >> |
  <d b>4\( <e c> <fis c a> <g d b> |
  <c f, c> <d f, d> <e, c g>2\) |
  
  << {e4\( d c <e c> |
      c'2 b4 <a fis>\) |
      d4.\( c8 <b g d>4 <d g, f> |
      <g e,> <e g,> d c\) |
      <b d,>\( <a c,> <b d,> <c e,> |
      <d fis,> <c e,> <a c,> <b d,>\) } \\
     {g,2 s |
      a'4 g2 s4 |
      <g d>4 <fis d> s2 |
      s2 g2 |
      g1 |
      s1 } \\
     {\voiceTwo s1 d1} >> |
  <g b,>1 \bar "||"

  \repeat unfold 17 { r1 }
  \repeat unfold 18 { r1 }
  
  <b d,>4\( <g e>_\markup\italic"accomp." <d' fis, c> <b fis d> |
  <g b,> <e c> <fis a,>_\> <d c> |
  <g e c>1~ |
  <g d b>1*2/4\) s2\! \bar "|."
}

pianoLH = \relative c' {
  \set Staff.midiInstrument = "acoustic grand"
  \set Staff.midiMinimumVolume = #0.5
  \set Staff.midiMaximumVolume = #0.5
  << {b4 c a s | s1} \\
     {<d, g,>1~ | q} >> |
  << {d1~ | d2 s2 | g2. s4 } \\
     {g,1~| g4 a <b g'>2 | c4. d8 e4 <a c> } >> |
  b2 a |
  
  << {s2 b4 d4 | c4 d2 s4 } \\
            {g,2. g4| c2 b4 <c a>} >>
  <d g,> <c e,> <b b,>4. b8~ |
  b4 a g fis |
  <d g,>2. <g, g,>4 |
  << {a4 b} \\ {g,2} >> <c c'>2 |
  
  c'4 b << {e2} \\ {a,4 g} >> |
  f e' d c |
  b a g <g' b> |
  <c c,> <a c,> <b d,> <c e,> |
  d,1 |
  << {d2\( e4 fis\)} \\ {d1} >> |
  <g d g,>1 \bar "||"
  
  \repeat unfold 17 { r1 }
  \repeat unfold 18 { r1 }
  \oneVoice<d g,>1~ | q | <g g,>~ | q
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
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWords
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
    \new PianoStaff << \new Staff { \new Voice { \global \pianoRH } } \new Staff { \clef "bass" \global \pianoLH } >>
  >>
  \layout {
%6.14 \context {\Lyrics\override LyricText #'font-size = #0.9 }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 4)
      %\override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 2)
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
  \midi {
    \tempo 4 = 90
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
