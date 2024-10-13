\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Jesus, the Light of the World"}}
  composer = \markup\oldStyleNum"Arranged by George D. Elderkin"
  tagline = \markup\concat { "from " \italic \oldStyleNum"The Finest of the Wheat No. 2" \oldStyleNum", 1894"}
}
\paper {
  %print-all-headers = ##f
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
  first-page-number = #019
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
  \time 6/8
  \autoBeamOff
}

sopMusic = \relative c'' {
  a4 c8 c4 a8 |
  a4 g8 f4. |
  a8 a8. a16 d8 c8. a16 |
  g4.~ g4 b8\rest |
  a4 c8 c4 a8 |
  a4 g8 f4. |
  
  a8 a8. f16 g8 g8. g16 |
  \partial 8*5 f4.~ f4 | \break
  \partial 8 c'8\fermata |
  a8 c c c4. |
  a8 c c c4. |
  a8 c8. c16
  
  d8 c a |
  a g g g4. |
  a8 c8. c16 c8 a f |
  g f d f4.
  a8 a8. f16 g8 g8. g16 |
  f2. \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  f4 f8 f4 f8 |
  e4 c8 c4. |
  f8 f8. f16 f8 f8. f16 |
  e4.~ e4 s8 |
  f4 f8 f4 f8 |
  e4 c8 c4. |
  
  f8 f8. c16 e8 e8. e16 |
  c4.~ c4 |
  f8 |
  f8 f f f4. |
  f8 f f f4. |
  f8 f8. f16 
  
  f8 f f |
  f d f e4. |
  f8 f8. f16 f8 f c |
  d d bes d4. |
  
  f8 f8. c16 e8 e8. e16 |
  c2. \bar "|."
}
altoWords = \lyricmode {
  \set stanza = #"1. "
  Hark! the Her -- ald an -- gels sing,
  Je -- sus, the Light of the world;
  Glo -- ry to the new -- born King,
  Je -- sus, the Light of the world.
  
  \dropLyricsIV
  We’ll walk in the light,
  beau -- ti -- ful light,
  Come where the dew -- drops of mer -- cy are bright,
  \raiseLyrics
  Shine all a -- round us by day and by night,
  Je -- sus, the Light of the world.
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
  Joy -- ful all ye na -- tions rise,
  Je -- sus, the Light of the world;
  Join the tri -- umph of the skies,
  Je -- sus, the Light of the world.
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
  Christ, by high -- est heav’n a -- dored,
  Je -- sus, the Light of the world;
  Christ, the ev -- er -- last -- ing Lord,
  Je -- sus, the Light of the world.
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
  Hail! the heav’n -- born Prince of peace,
  Je -- sus, the Light of the world;
  Hail! the sun of right -- eous -- ness,
  Je -- sus, the Light of the world.
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
  c4 a8 a4 c8 |
  c4 bes8 a4. |
  c8 c8. c16 bes8 a8. c16 |
  c4.~ c4 s8 |
  c4 a8 a4 c8 |
  c4 bes8 a4. |
  
  c8 c8. a16 bes8 bes8. bes16 |
  a4.~ a4 |
  a8 |
  c8 a a a4. |
  c8 a a a4. |
  c8 a8. a16 
  
  bes8 a c |
  c b d c4. |
  c8 a8. a16 a8 c a |
  bes bes f f4. |
  
  c'8 c8. a16 bes8 bes8. bes16 |
  a2. \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  f4 f8 f4 f8 |
  c4 c8 f4. |
  f8 f8. f16 f8 f8. f16 |
  c4.~ c4 d8\rest |
  f4 f8 f4 f8 |
  c4 c8 f4. |
  
  c8 c8. c16 c8 c8. c16 |
  f4.~ f4 |
  f8\fermata |
  f8 f f f4. |
  f8 f f f4. |
  f8 f8. f16
  
  f8 f f |
  f g g c,4. |
  f8 f8. f16 f8 f f |
  bes, bes bes bes4. |
  
  c8 c8. c16 c8 c8. c16 |
  f2. \bar "|."
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
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWords
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
