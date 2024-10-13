\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"The Seven Joys of Mary"}}
  poet = \markup\oldStyleNum"Traditional"
  %composer = \markup { \center-column { "Old English" \oldStyleNum"Arranged by Sir John Stainer (1840–1901)"}}
  composer = \markup\oldStyleNum"Old English"
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
       (padding . -20)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #066
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
#(set-global-staff-size 14.5) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 14.5 20))) }
global = {
  \key g \major
  \time 6/8
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(0 . 0)
}

sopMusic = \relative c' {
  \partial 8 d8^\mf |
  g4 g8 g4 a8 |
  b4 a8 g4 g8 |
  a4 a8 fis4 fis8 | \break
  
  g4.~ g4 d8 |
  g4 g8 g4 a8 |
  b4 a8 g4 | \bar""\break g8 |
  
  \slurDotted a8( a) a8 fis8[ e] fis |
  g4.~ g4 \bar "||"
  g8^\f |
  a8( a) a8 a4 a8 |
  a4 | \bar""\break
  
  \slurSolid
  b8 c[ b] a |
  b4 b8 b4 a8 |
  b4( c8 d4\fermata) d16[^\ff c] |
  
  b4 a8 g4 c8 |
  b4 a8 g4 g8 |
  a4 a8 fis8[ e] fis |
  \partial 8*5 g4. b4\rest \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d8 |
  d4 d8 e4 d8 |
  d4 fis8 g4 g8 |
  e4 e8 d4 d8 |
  
  d4.~ d4 d8 |
  d4 d8 e4 d8 |
  d4 fis8 g4 g8 |
  
  \slurDotted
  e8( e) e8 d4 d8 |
  d4.~ d4 |
  e8 |
  d8( d) d8 d4 cis8 |
  d4 |
  
  \slurSolid
  d8 e4 fis8 |
  g4 g8 g4 fis8 |
  g4.( fis4) d8 |
  
  d4 dis8 e4 g8 |
  g4 fis8 e4 e8 |
  e4 e8 d4 d8 |
  d4. s4 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsVII
  \set stanza = #"1. "
  The first good joy that Ma -- ry had, It was the joy of
  \set stanza = "1. "
  one; __
  To see the bless -- ed Je -- sus Christ,
  \set stanza = "1. "
  When He __ was first her Son. __
  When He __ was first her Son,
  
  Good Lord; And hap -- py may we be; __
  Praise Fa -- ther, Son, and Ho -- ly Ghost To all e -- ter -- ni -- ty.
}
altoWordsII = \lyricmode {
  \dropLyricsVII
  \set stanza = #"2–7. "
  The next good joy that Ma -- ry had, It was the joy of
  \set stanza = "2. "
  two; __
  To see her own Son Je -- sus Christ,
  \set stanza = "2. "
  \set ignoreMelismata = ##t
  "" Mak -- ing the lame _ to go. __ _
  "" Mak -- ing the lame to go,
}
altoWordsIII = \lyricmode {
  \dropLyricsVII
  \repeat unfold 13 { \skip 1}
  \set stanza = "3. "
  three; __ To see her own Son Je -- sus Christ,
  \set stanza = "3. "
  \set ignoreMelismata = ##t
  "" Mak -- ing the blind _ to see. __ _
  "" Mak -- ing the blind to see,
}
altoWordsIV = \lyricmode {
  \dropLyricsVII
  \repeat unfold 13 { \skip 1}
  \set stanza = "4–7. " "four, five, etc.;" To see her own Son Je -- sus Christ,
  \set stanza = "4. "
  \set ignoreMelismata = ##t
  "" Read -- ing the Bi -- _ ble o’er. __ _
  "" Read -- ing the Bi -- ble o’er,
}
altoWordsV = \lyricmode {
  \dropLyricsVII
  \repeat unfold 22 { \skip 1}
  \set stanza = "5. "
  \set ignoreMelismata = ##t
  "" Rais -- ing the dead _ to life. __ _
  "" Rais -- ing the dead to life,
}
altoWordsVI = \lyricmode {
  \dropLyricsVII
  \repeat unfold 22 { \skip 1}
  \set stanza = "6. "
  Up -- on __ the Cru -- ci -- fix. __
  Up -- on __ the Cru -- ci -- fix,
}
altoWordsVII = \lyricmode {
  \dropLyricsVII
  \repeat unfold 22 { \skip 1}
  \set stanza = "7. "
  A -- scend -- ing in -- to heav’n. __
  A -- scend -- ing in -- to heav’n,
}
tenorMusic = \relative c' {
  d8_\mf |
  b4 b8 b4 a8 |
  g4 c8 b4 b8 |
  c4 c8 a4 a8 |
  
  b4.~ b4 d8 |
  b4 b8 b4 a8 |
  g4 c8 b4 b8 |
  
  \slurDotted
  c8( c) c8 a4 a8 |
  b4.~ b4 |
  b8_\f |
  a8( a) a8 a4 g8 |
  fis4 |
  
  \slurSolid
  g8 g4 d'8 |
  d4 d8 d4 d8 |
  d4( g,8 a4) a8_\ff |
  
  g4 a8 b4 e8 |
  d4 c8 b4 b8 |
  c4 c8 a4 a8 |
  b4. s4 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  d8 |
  g4 g8 e4 fis8 |
  g4 d8 e4 e8 |
  c4 c8 d4 d8 |
  
  g4.~ g4 d8 |
  g4 g8 e4 fis8 |
  g4 d8 e4 e8 |
  
  \slurDotted
  c8( c) c8 d4 d8 |
  g4.~ g4 |
  e8 |
  fis8( fis) fis8 fis4 e8 |
  d4 |
  
  \slurSolid
  g8 c,4 d8 |
  g4 g8 g4 d8 |
  g4( e8 d4\fermata) fis8 |
  
  g4 fis8 e4 c8 |
  d4 d8 e4 e8 |
  c4 c8 d4 d8 |
  g4. d4\rest \bar "|."
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
    \new Lyrics = "altosVII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsVII
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
    \tempo 4 = 120
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
