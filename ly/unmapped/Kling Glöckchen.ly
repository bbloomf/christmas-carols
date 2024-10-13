\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Kling Glöckchen"}}
  poet = \markup\oldStyleNum"Karl Enslin (1819–1875)"
  composer = \markup\oldStyleNum"Traditional German"
  tagline = \markup \concat { "from " \italic "The Wartburg Hymnal" \oldStyleNum", 1918"}
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
%{IF_LESSER
  markup-system-spacing #'stretchability = 50
  top-markup-spacing #'stretchability = 30
  last-bottom-spacing #'stretchability = 60
%}%END_IF_LESSER
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #172
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle christmas}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine 
        \fill-line{"" \if \should-print-page-number
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")
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
  \override DynamicLineSpanner #'Y-extent = #'(0 . 0)
}

sopMusic = \relative c'' {
  d2 b4 c |
  d8 e d e d4 b\rest |
  c2 a4 d |
  b2 b\rest | \break
  a4 a b g |
  
  b2 a |
  c4 c d a |
  c2 b |
  a4 a b cis |
  d2 a |
  
  b4 e d cis |
  e2 d4 b\rest | \break
  d2 b4 c |
  d8 e d e d4 b\rest |
  c2 a4 d |
  g,2 b\rest |
  \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  g'2 d4 fis |
  g8 g g fis g4 s |
  fis2 fis4 fis |
  g2 s |
  fis4 fis g d |
  
  g2 fis |
  fis4 fis a fis |
  a2 g |
  g4 g g g |
  fis2 fis |
  
  g4 g fis e |
  g2 fis4 s |
  g2 d4 fis |
  g8 g g fis g4 s |
  e2 fis4 fis |
  g2 s \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  Kling, Glöck -- chen kling -- e -- ling -- e -- ling!
  Kling, Glöck -- chen kling!
  
  \set stanza = #"1. "
  Laßt mich ein, ihr Kind -- er!
  Ist so kalt der Win -- ter!
  Öff -- net mir die Tü -- ren!
  Laßt mich nicht er -- frie -- ren!
  
  Kling, Glöck -- chen kling -- e -- ling -- e -- ling!
  Kling, Glöck -- chen kling!
}
altoWordsII = \lyricmode {
  \dropLyricsV
%\markup\italic
  %Kling, Glöck -- chen kling -- e -- ling -- e -- ling!
  %Kling, Glöck -- chen kling!
  \repeat unfold 12 \skip1
  \set stanza = #"2. "
  Mäd -- chen, hört, und Büb -- chen,
  Macht mir auf das Stüb -- chen!
  Bring euch vie -- le Ga -- ben,
  Sollt euch dran er -- la -- ben!
  
  %Kling, Glöck -- chen kling -- e -- ling -- e -- ling!
  %Kling, Glöck -- chen kling!
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  %Kling, Glöck -- chen kling -- e -- ling -- e -- ling!
  %Kling, Glöck -- chen kling!
  \repeat unfold 12 \skip1
  \set stanza = #"3. "
  Hell er -- glühn die Ker -- zen,
  Öff -- net mir die Her -- zen,
  Will drin woh -- nen fröh -- lich,
  From -- mes Kind, wie se -- lig!
  
  %Kling, Glöck -- chen kling -- e -- ling -- e -- ling!
  %Kling, Glöck -- chen kling!
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
  b2 b4 d |
  b8 c b c b4 s |
  d2 d4 d |
  d2 s |
  d4 d d b |
  
  d2 d |
  d4 a d d |
  d2 d |
  cis4 cis d e |
  d2 d |
  
  g,4 b a a |
  cis2 d4 s |
  b2 b4 d |
  b8 c b c b4 s |
  a2 d4 c |
  b2 s |
  \bar "|."
}
tenorWords = \lyricmode {
}
tenorWordsII = \lyricmode {
}

bassMusic = \relative c {
  g'2 g4 d |
  g8 g g d g4 d\rest |
  a'2 d,4 d |
  g2 d\rest |
  d4 d g, g' |
  
  d2 d |
  d4 d fis d |
  g2 g |
  a4 a a a |
  d,2 d |
  
  g4 e a a |
  a2 d,4 d\rest |
  g2 g4 d |
  g8 g g d g4 d\rest |
  c2 d4 d |
  g2 d\rest \bar "|."
}
bassWordsII = \lyricmode {
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
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women"  \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women"  \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)(padding . -0.35)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)(padding . -0.35)) } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)(padding . -0.35)) } \lyricsto "basses" \bassWordsII
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)(padding . -0.35)) } \lyricsto "basses" \bassWords
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
