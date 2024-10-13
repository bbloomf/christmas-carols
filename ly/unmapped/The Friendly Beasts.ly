\version "2.14.2"
\include "util.ly"
\header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"The Friendly Beasts"}}
    poet = \markup\oldStyleNum"Robert Davis (1881–1950)"
    composer = \markup\concat{"Adapted from "\italic"Orientis Partibus" \oldStyleNum", 12th Century French"}
    tagline = \markup { "from" \italic {HymnsAndCarolsOfChristmas.com}}
  }\paper {
  print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 10)
       (minimum-distance . 10)
       (padding . 1.5)
       (stretchability . 100))
  score-markup-spacing = 
    #'((basic-distance . 5)
       (minimum-distance . 5)
       (padding . 1)
       (stretchability . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #078
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
  \key f \major
  \time 3/4
  \autoBeamOff
}

sopMusic = \relative c' {
  \repeat volta 3 {
    f4 f g |
    a2 a4 |
    \slurDotted g( g) e |
    f2 c'4 |
    c( c) c |
    
    d2 d8\noBeam( d) |
    c2 c4 |
    a2 \bar"" a8\noBeam( a) |
    a4( a) g |
    bes4( bes) a |
    
    g( g) f |
    a2. |
    c4 c bes |
    a2 f4 |
    g( g) e |
    f2.
  }
  \break
  
  \repeat volta 4 {
    f4 f g |
    a2 a8( a) |
    \slurDotted g4( g) e |
    f2 c'8( c) |
    c4( c) c |
    
    d4( d) d8\noBeam( d) |
    c2 c4 |
    a2 \bar"" a8\noBeam( a) |
    a4( a) g |
    bes4( bes) a |
    
    g2 f4 |
    a2. |
    c4 c bes |
    a2 f4 |
    g( g) e |
    f2.
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  \slurDotted
  c4 d e |
  f2 c4 |
  d4( e) c |
  c2 e4 |
  
  f( f) f |
  f2 f8( f) |
  f2 e4 |
  f2 f8( f) |
  
  f4( f) c4 |
  f4( f) f4 |
  d4( e) d4 |
  e2. |
  
  f4 f f |
  \slurSolid
  e( d) d |
  \slurDotted
  d( e) c |
  c2.
  
  
  
  \slurDotted
  c4 d e |
  f2 c8( c) |
  d4( e) c |
  c2 e8( e) |
  
  f4( f) f |
  f4( f) f8( f) |
  f2 e4 |
  f2 f8( f) |
  
  f4( f) c4 |
  f4( f) f4 |
  \slurSolid d4( e) d4 |
  e2. |
  
  f4 f f |
  e( d) d |
  \slurDotted
  d( e) c |
  c2.
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  Je -- sus, our bro -- ther, kind _ and good, Was hum -- _ bly born in a sta -- ble rude,
  And the friend -- _ ly beasts _ a -- round _ Him stood; Je -- sus, our broth -- _ er, kind _ and good.
  
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
  “I,” said the sheep with _ curl -- _ y horn, “I __ _ gave Him my wool _ for His blank -- et warm,
  He __ _ wore __ _ my coat _ on Christ -- _ mas morn.” “I,” said the sheep _ with curl -- _ y horn.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  “I,” said the don -- key, shag -- gy and brown, “I car -- ried His Moth -- er up hill and down;
  I __ _ car -- ried Her safe -- ly to Beth -- le -- hem town.” “I,” said the don -- _ key, shag -- gy and brown.
  
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
  “I,” said the dove from the raf -- _ ters high,
  "" "" “Cooed Him to sleep, _ that He should not cry,
  We _ cooed Him to sleep, _ my mate _ and I.”
  “I,” said the dove _ from the raf -- ters high.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  “I,” said the cow, all white _ and red, “I gave Him my man -- ger __ _ for His bed,
  I __ _ gave Him my hay __ _ to pil -- low His head.” “I,” said the cow, _ all white _ and red.
  
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
  “I,” said the cam -- el, __ _ yel -- low and black,
  "" "" “O -- ver the des -- ert, up -- _ on my back
  I __ _ brought Him a gift in the Wise _ Men’s pack,”
  “I,” said the cam -- _ el, yel -- low and black.
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \repeat unfold 32 \skip1
  \set stanza = #"7. "
  \set ignoreMelismata = ##t
  Thus ev -- ’ry beast by __ _ some _ good spell,
  In the sta -- _ ble dark _ was __ _ glad to tell
  Of the gift __ _ he gave _ Em -- man -- _ u -- el,
  The gift he gave _ Em -- man -- _ u -- el.
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
altoWordsVII = \lyricmode {
}
tenorMusic = \relative c' {
  \slurDotted
  a4 a bes |
  c2 a4 |
  bes( bes) g |
  a2 g4 |
  
  a( a) a |
  \slurSolid
  bes( a) \slurDotted f8( f) |
  g2 g4 |
  a2 c8( c) |
  
  c4( c) c4 |
  bes4( bes) c4 |
  d4( d) d4 |
  cis2. |
  
  a4 a d |
  \slurSolid
  c( bes) a |
  \slurDotted
  bes( a) g |
  a2.
  
  
  
  \slurDotted
  a4 a bes |
  c2 a8( a) |
  bes4( bes) g |
  a2 g8( g) |
  
  a4( a) a |
  bes( a) f8( f) |
  g2 g4 |
  a2 c8( c) |
  
  c4( c) c4 |
  bes4( bes) c4 |
  d2 d4 |
  cis2. |
  
  a4 a d |
  \slurSolid
  c( bes) a |
  \slurDotted
  bes( a) g |
  a2.
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  \slurDotted
  f4 d c |
  f2 f,4 |
  bes4( c) c4 |
  f2 c4 |
  
  f( f) f |
  bes,2 d8( d) |
  c2 c4 |
  f2 f8( f) |
  
  f4( f) e4 |
  d4( d) f4 |
  bes,4( c) d4 |
  a2. |
  
  f4 f' f |
  c2 d4 |
  bes( c) c4 |
  f2.
  
  
  
  
  \slurDotted
  f4 d c |
  f2 f,8( f) |
  bes4( c) c4 |
  f2 c8( c) |
  
  f4( f) f |
  bes,4( bes) d8( d) |
  c2 c4 |
  f2 f8( f) |
  
  f4( f) e4 |
  d4( d) f4 |
  \slurSolid
  bes,4( c) d4 |
  a2. |
  
  f4 f' f |
  c2 d4 |
  \slurDotted
  bes( c) c4 |
  f2.
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
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWordsVII
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWords
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
%{IF_LESSER
\context {
  \Lyrics
  \override LyricText #'font-size = #1.2
}
%}%END_IF_LESSER
%6.14 \context {\Lyrics\override LyricText #'font-size = #0.8 }
    #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 14.4 20)))
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 2)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 2)
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
    \tempo 4 = 105
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
