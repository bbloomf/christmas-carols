\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Gloucestershire Wassail"}}
  poet = \markup\oldStyleNum"18th Century English"
  composer = \markup\oldStyleNum"18th Century English"
  %arranger = \markup\oldStyleNum"Arranged by BHB"
  tagline = \markup ""
}
\paper {
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -15)
       (stretchability . 100))
  last-bottom-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #074
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"christmas"}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \if \should-print-page-number
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \if \should-print-page-number
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key g \major
  \time 3/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \tempo"Allegro" 
  \repeat volta 2 {
    \partial 4 d4 |
    \slurDashed g4( g) g |
    g( a) b |
    c b a |
    \slurSolid b( d) d4 |
    
    c4 a a |
    \slurDashed a( b) c |
    \slurSolid b8[ a] g[ a] b4 |
    a2 c4 |
    
    b8[ a] g[ a] b[ c] |
    d2 \slurDashed d8\noBeam( c) |
    b4 g b |
    a2 g8\noBeam a |
    
    b2 a8\noBeam b |
    c2 b4 |
    a g fis |
    \partial 2 g2
  }
  
  
  \repeat volta 2 {
    \partial 4 d4 |
    \slurDashed g4 g g |
    g a b |
    c b a |
    \slurSolid b( d) \slurDashed d8( d) |
    
    c4 a a |
    \slurDashed a( b) c |
    \slurSolid b8[ a] g[ a] b4 |
    a2 c4 |
    
    b8[ a] g[ a] b[ c] |
    d2 \slurDashed d8\noBeam( c) |
    b4 g b |
    a2 g8\noBeam( a) |
    
    b2 a8\noBeam( b) |
    c2 b4 |
    a( g) fis |
    \partial 2 g2
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d4 |
  \slurDashed d( d) e|
  d( fis) g|
  g g fis |
  g2 g4 |
  
  a4 fis fis |
  fis( d) fis |
  \slurSolid g8[ fis] e[ fis] g4 |
  fis2 fis4 |
  
  g8[ fis] e[ fis] g4 |
  fis2 \slurDashed fis8\noBeam( fis) |
  g4 g g |
  fis2 e8\noBeam fis |
  
  g2 fis8\noBeam g |
  g2 g4 |
  fis d4. c8 |
  b2
  
  
  
  
  d4 |
  \slurDashed d d e|
  d fis g|
  g g fis |
  g2 g8( g) |
  
  a4 fis fis |
  fis( d) fis |
  \slurSolid g8[ fis] e[ fis] g4 |
  fis2 fis4 |
  
  g8[ fis] e[ fis] g4 |
  fis2 \slurDashed fis8\noBeam( fis) |
  g4 g g |
  fis2 e8\noBeam( fis) |
  
  g2 fis8\noBeam( g) |
  g2 g4 |
  fis( d4.) c8 |
  b2
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1."
  \set ignoreMelismata = ##t
  Was -- sail, __ _ was -- sail __ _ all o -- ver the town, _
  Our toast it is white and our ale _ it __ _ is brown;
  Our bowl _ it __ _ is __ _ made of the white ma -- ple tree,
  With the was -- sail -- ing bowl we’ll drink un -- to thee.
  \unset ignoreMelismata
  
  
  \set stanza = #"  4."
  \set ignoreMelismata = ##t
  And here is to Fill -- pail and to her left ear, __ _
  Pray __ _ God send our mas -- ter a hap -- _ py _ New Year,
  A hap -- _ py _ New _ Year as __ _ e’er he did see,
  With the was -- sail -- ing bowl we’ll drink un -- to thee.
}
altoWordsII = {
  \dropLyricsIX
  \set stanza = \markup{\dynamic"mf " "2."}
  \lyricmode {
  %\markup\italic
    \set ignoreMelismata = ##t
    So here is to Cher -- ry and to his right cheek, _
    Pray God send our mas -- ter a good _ piece _ of beef,
    A good _ piece _ of __ _ beef that _ may we all see,
    With the was -- sail -- ing bowl we’ll drink un -- to thee.
  
    
    \set stanza = #"  5."
    \set ignoreMelismata = ##t
    Come but -- ler, come fill us a bowl of the best, _
    Then we hope that your soul _ in heav -- _ en _ may rest,
    But if __ _ you _ do _ draw us a bowl of the small,
    Then _ down shall go but -- ler, bowl __ _ and all.
  }
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3."
  \set ignoreMelismata = ##t
  And here is to Dob -- bin and to his right eye, __ _
  Pray God send our mas -- ter a good _ Christ -- _ mas pie,
  A good _ Christ -- _ mas _ pie that _ may we all see,
  With the was -- sail -- ing bowl we’ll drink un -- to thee.
  
  
  \set stanza = #"  6."
  \set ignoreMelismata = ##t
  Then here’s to the maid in the li -- ly white smock, _
  Who _ tripp’d to the door _ and slipp’d _ back _ the lock,
  Who tripp’d _ to _ the _ door and _ pulled back the pin,
  For to let these _ jol -- ly was -- sail -- ers in.
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  b4 |
  \slurDashed b( b) c |
  b( c) d |
  e d c |
  \slurSolid d( b) b4 |
  
  \slurDashed
  e4 d c |
  d( g,) a |
  g e' d |
  d2 d4 |
  
  g, e' d8[ c] |
  d2 a8\noBeam( a) |
  g8[ d'] b4 d |
  d2 b8\noBeam d |
  
  g,2 d'8\noBeam g, |
  e'2 d4 |
  c b a |
  g2
  
  
  
  
  b4 |
  \slurDashed b b c |
  b c d |
  e d c |
  \slurSolid d( b) \slurDashed b8( b) |
  
  \slurDashed
  e4 d c |
  d( g,) a |
  g e' d |
  d2 d4 |
  
  g, e' d8[ c] |
  d2 a8\noBeam( a) |
  g8[ d'] b4 d |
  d2 b8\noBeam( d) |
  
  g,2 d'8\noBeam( g,) |
  e'2 d4 |
  c( b) a |
  g2
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  g'4 |
  \slurDashed g4( g) c, |
  g'( d) g |
  c, g' d |
  g2 g4 |
  
  a4 d, d |
  d( g) d |
  g b g |
  d2 d4 |
  
  g4 b g8[ e] |
  d2 \slurDashed d8\noBeam( d) |
  g4 g g |
  d2 e8\noBeam d |
  
  g2 d8 g |
  c,2 g'4 |
  d d d |
  g,2
  
  
  
  
  g'4 |
  \slurDashed g4 g c, |
  g' d g |
  c, g' d |
  g2 g8( g) |
  
  a4 d, d |
  d( g) d |
  g b g |
  d2 d4 |
  
  g4 b g8[ e] |
  d2 \slurDashed d8\noBeam( d) |
  g4 g g |
  d2 e8\noBeam( d) |
  
  g2 d8( g) |
  c,2 g'4 |
  d( d) d |
  g,2
}
bassWords = \lyricmode {

}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic>> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "sopranos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText.font-size = #1.3
  }
    \context {
      \Score
      %\override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/8)
      %\override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
}

