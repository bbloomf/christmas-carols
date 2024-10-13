\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Joseph, O Dear Joseph Mine"}}
  poet = \markup\concat{\italic"Josef, Lieber Josef Mein" \oldStyleNum", 16th Century"}
  composer = \markup\concat{ \italic"Resonet in laudibus" \oldStyleNum", 14th Century"}
  tagline = ""
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
  first-page-number = #038
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
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(0 . 1.5)
  \override DynamicText #'X-offset = #-3
}

sopMusic = \relative c'' {
  \once\override DynamicText #'X-offset = #-5
  c4_\mf a8 f4 a8 |
  c4 d8 c4. |
  c4 a8 f4 a8 |
  c4 d8 c4. | \break
  
  bes4 a8 g4 c8 |
  bes4 a8 g4 a8 |
  c4 a8 f4 a8 |
  g4 f8 g4 f8 | \break
  
  f2. |
  f2. |
  c'4._\f a |
  c_\p a |
  c2. |
  a |
  
  f4 g8 a4 bes8 |
  a[ f] g a4. |
  f4 g8 a4 bes8 |
  a[ f] g a4 a8 |
  
  f4.~ f4 f8 |
  f2. |
  c'4^\markup\italic"cresc." c8 d4 e8 |
  f4 e8 d4 a8 |
  
  c4. b |
  c2. |
  f,4. g |
  a bes |
  a2.( |
  g4.)( g4) f8 |
  f2. \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  f4 e8 d4 f8 |
  e4 f8 f4. |
  f4 e8 d4 f8 |
  e4 f8 f4. |
  
  e4 f8 g4 e8 |
  f4 f8 e4 f8 |
  e4 e8 f4 f8 |
  d4 d8 d4 d8 |
  
  c4.( bes) |
  c2. |
  e4. f |
  e f |
  g2. |
  f2. |
  
  d4 e8 f4 f8 f4 e8 e4. |
  d4 e8 f4 f8 |
  f4 e8 e4 e8 |
  
  d4.~ d4 d8 |
  c2. |
  f4 a8 f4 bes8 |
  a4 a8 a4 a8 |
  
  g4. g |
  g2. |
  f4.^\< e |
  f\! f |
  f2.( |
  f4.)(^\> e4) c8 |
  <<c2. {s8 s\! s2}>> \bar "|."
}
altoWords = {
  \lyricmode {
    \dropLyricsIV
    \set stanza = #"1. "
    Jo -- seph, O dear Jo -- seph mine,
    Help me rock the Child di -- vine,
    God re -- ward both thee and thine,
    In par -- a -- dise, So prays the moth -- er,
    
    \raiseLyrics
    Ma -- ry.
  }
  %\set stanza = \markup\dynamic"f "
  \lyricmode {
    E -- ia,
  }
  %\set stanza = \markup\dynamic"p "
  \lyricmode {E -- ia, E -- ia.
    He came down at Christ -- mas time,
    In the town of Beth -- le -- hem, in Beth -- le -- hem.
    Bring -- ing to men far and wide,
    Love’s di -- a -- dem,
    E -- ia, E -- ia, Lul -- la -- by.
  }
}
altoWordsII = \lyricmode {
  \dropLyricsIV
%\markup\italic
  \set stanza = #"2. "
  I will glad -- ly, la -- dy mine,
  Help thee rock the Child di -- vine,
  God’s pure light on thee will shine,
  In par -- a -- dise, So prays the moth -- er,
}
altoWordsIII = \lyricmode {
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  \once\override DynamicText #'X-offset = #-5
  a4^\mf c8 a4 f8 |
  g4 bes8 a4. |
  c4 c8 a4 a8 |
  g4 bes8 a4. |
  
  c4 c8 c4 g8 |
  d'4 c8 c4 c8 |
  g4 a8 a4 a8 |
  bes4 a8 bes4 bes8 |
  
  bes4.( g) |
  a2. |
  c4.^\f c |
  c^\p d |
  e2. |
  c |
  
  a4 c8 c4 d8 |
  d4 d8 c4. |
  a4 c8 c4 d8 |
  d4 d8 c4 c8 |
  
  a4.~ a4 bes8 |
  a2. |
  a4_\markup\italic"cresc." c8 bes4 c8 |
  c4 c8 d4 d8 |
  
  e4. d |
  e2. |
  a,4._\< c |
  c\! bes |
  c2.( |
  c4.)(_\> bes4) a8 |
  <<a2. {s8 s\! s2}>> \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  f4 c8 d4 d8 |
  c4 bes8 f'4. |
  a,4 c8 d4 d8 |
  c4 bes8 f'4. |
  
  g4 f8 e4 c8 |
  d4 f8 c4 f8 |
  c4 c8 d4 d8 |
  bes4 d8 bes4 bes8 |
  
  f'2. |
  f |
  c4. f |
  a d, |
  c'2. |
  f, |
  
  d4 c8 f4 bes,8 |
  d4 bes8 a4. |
  d4 c8 f4 bes,8 |
  d4 bes8 a4 a8 |
  
  d4.~ d4 bes8 |
  f'2. |
  f4 f8 bes4 g8 |
  f4 f8 f4 f8 |
  
  g4. g, |
  c2. |
  d4. c |
  f d |
  c2.( |
  c4.)( c4) f8 |
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
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
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
