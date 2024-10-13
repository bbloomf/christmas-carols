\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Auld Lang Syne"}}
  poet = \markup\oldStyleNum"First verse, traditional"
  meter = \markup\oldStyleNum"Other verses, Robert Burns (1759–1796)"
  composer = \markup\oldStyleNum"Traditional"
  tagline = \markup \concat{ "from " \italic "Favorite Songs and Hymns for School and Home" \oldStyleNum", 1899"}
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
  first-page-number = #201
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"new year"}
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
  \time 2/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \partial 8 d8^\p |
  g8. g16 g8 b |
  a8. g16 a8 b |
  g8. g16 b8 d |
  e4. \bar""\break e8 |
  d8. b16 b8 g |
  
  a8. g16 a8 b |
  \slurDotted g8.( e16) e8( d) |
  \partial 4. g4 b8\rest \bar "|:" \break
  \partial 8 e8 |
  d8.[ b16] b8[ g] |
  a8. g16 a8 b16\rest b |
  
  b8.[ g16] b8.[ d16] |
  e4. \bar""\break e8 |
  d8. b16 b8 g |
  a8. g16 a8 b |
  g8.[ e16] e8[ d] |
  \partial 4. g4  b8\rest \bar ":|"
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d8 |
  d8. d16 d8 g |
  fis8. e16 fis8 fis |
  d8. d16 g8 g |
  g4. g8 |
  g8. g16 g8 g |
  
  fis8. e16 fis8 g |
  \slurDotted e8.( c16) c8( c) |
  b4 s8 |
  g'8 |
  g4 g |
  fis8. e16 fis8 s16 g |
  
  g8.[ e16] g8.[ b16] |
  c4. c8 |
  b8. g16 g8 g |
  fis8. e16 fis8 fis |
  e4 c4 |
  b4 s8 \bar ":|"
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  Should auld ac -- quain -- tance be for -- got,
  And nev -- er brought to mind?
  Should auld ac -- quain -- tance be for -- got,
  And days of auld lang syne?

  \unset ignoreMelismata
  \set associatedVoice = "sopranos"
  For auld lang syne, my dear,
  For auld lang syne;
  We’ll tak’ a cup o’ kind -- ness yet
  For auld lang syne.
  
}
altoWordsII = \lyricmode {
  \dropLyricsV
%\markup\italic
  \set stanza = #"2. "
  We twa ha’e run a -- boot the braes,
  And pu’d the gow -- ans fine;
  But_we’ve wan -- der’d mon -- y~a wea -- ry foot,
  Sin’ auld __ lang __ syne.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  We twa ha’e sport -- ed i’ the burn,
  Frae morn -- in’ sun till dine,
  But seas be -- tween us braid ba’e roared
  Sin’ auld __ lang __ syne.
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = #"4. "
  And here’s a hand, my trust -- y frien’,
  And gie’s a hand o’ thine;
  We’ll tak’ a cup o’ kind -- ness yet,
  For auld __ lang __ syne.
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
  b8 |
  b8. b16 b8 d |
  d8. d16 d8 d |
  b8. b16 d8 b |
  c4. c8 |
  b8. d16 d8 b |
  
  d8. d16 d8 d |
  \slurDotted c8.( g16) fis8( fis) |
  g4 s8 |
  c |
  b8.[ d16] d4 |
  d8. d16 d8 s16 d |
  
  d4 d |
  c4. c8 |
  d8. d16 d8 b |
  d8. d16 d8 d |
  b4 fis |
  g s8 \bar ":|"
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  g'8 |
  g8. g16 g8 g |
  d8. d16 d8 d |
  g8. g16 g8 g |
  c,4. c8 |
  g'8. g16 g8 g |
  
  d8. d16 d8 g |
  \slurDotted c,8.( c16) d8( d) |
  g,4 d'8\rest |
  g |
  g4 g4 |
  d8. d16 d8 d16\rest g |
  
  g4 g |
  c,4. c8 |
  g'8. g16 g8 g |
  d8. d16 d8 b |
  e4 d |
  g, d'8\rest \bar ":|"
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
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWords
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
    \tempo 4 = 60
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
