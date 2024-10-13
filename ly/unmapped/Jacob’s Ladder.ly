\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Jacob’s Ladder"}}
  poet = \markup\oldStyleNum"Traditional"
  composer = \markup\oldStyleNum"Traditional"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
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
       (padding . -15)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #167
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
  \time 2/4
  \autoBeamOff
  \slurDotted
}

sopMusic = \relative c' {
  \partial 8 d8 |
  g4 g8 g |
  g[ b] a g |
  a4 a8 a |
  a[ c] b( a) |
  
  g4 g8 g |
  g[ b] a g |
  a4 d,8 d |
  d4 b'8( c) |
  d4 d8 e |
  
  d4 a8 b |
  c4 c8 c |
  \slurDotted
  c4 b8( c) |
  d4 d8 e |
  d[ b] c a|
  
  g4 g8 g |
  \partial 4 g4 \bar "||" \break
  \partial 4 d8 d |
  g4 g8 g |
  g[ b] a g |
  a4 a8 a |
  
  a[ c] b a |
  g4 g8 g |
  g[ b] a g |
  a4 d,8 d |
  
  d4 b'8 c |
  d4 d8 e |
  d[ b] c a |
  g4 g8 g |
  \partial 4. g4. \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d8 |
  d[ e] b c |
  d4 d8 cis |
  d4 d8 cis |
  d4 dis8( dis) |
  
  e4 b8 c |
  d4 cis8 cis |
  d4 d8 d |
  d4 d8( d) |
  g4 g8 g |
  
  fis4 f8 f |
  e4 ees8 ees8 |
  \slurDotted
  d4 d8( c) |
  g'4 g8 g |
  g[ f] e fis |
  
  g4 f8 e |
  d4 \bar "||"
  d8 d |
  d4 d8 d |
  g4 g8 g |
  fis4 e8 e |
  
  dis4 dis8 dis |
  g[ e] d c |
  b4 cis8 cis |
  d4 d8 d |
  
  d4 g8 fis |
  g4 g8 g |
  g4 g8 fis |
  g4 f8 e |
  d4. \bar "|."
}
dropLyrics = {
  \dropLyricsXI
}
altoWords = \lyricmode {
  \dropLyrics
  \set stanza = #"1. "
  As Ja -- cob with tra -- vel was wea -- ry one
  \set associatedVoice = "sopranos"
  day,
  At __ night on a stone __ for a pil -- low he lay,
  He __ saw in a vi -- sion a lad -- der so high,
  \set ignoreMelismata = ##t
  That its foot was on \unset ignoreMelismata
  earth, and its top in the sky.
  
  Hal -- le -- lu -- jah to Je -- sus, who died on the Tree,
  And hath rais’d up a lad -- der of mer -- cy for me,
  And hath rais’d up a lad -- der of mer -- cy for me.
}
altoWordsII = \lyricmode {
  \dropLyrics
%\markup\italic
  \set stanza = #"2. "
  This lad -- der is long, it is strong and well -- 
  \set associatedVoice = "sopranos"
  made,
  \set ignoreMelismata = ##t
  Has stood hun -- dreds of years __ _ and is not yet de -- cayed;
  Ma -- ny mil -- lions have climbed it and reached Si -- on’s hill,
  And __ _ thou -- sands by faith _ are __ _ climb -- ing it still.
}
altoWordsIII = \lyricmode {
  \dropLyrics
  \set stanza = #"3. "
  Come let __ us a -- scend: all may climb it who 
  \set associatedVoice = "sopranos"
  will;
  \set ignoreMelismata = ##t
  For the An -- gels of Ja -- _ cob are guard -- ing it still:
  And re -- mem -- ber each step, that by faith we pass o’er,
  Some _ Pro -- phet or Mar -- _ tyr hath trod it be -- fore.
}
altoWordsIV = \lyricmode {
  \dropLyrics
  \set stanza = #"4. "
  And when we ar -- rive at the ha -- ven of 
  \set associatedVoice = "sopranos"
  rest
  \set ignoreMelismata = ##t
  We shall hear the glad words, _ “Come up hi -- ther, ye blest,
  Here are re -- gions of light, here are man -- sions of bliss:”
  O, __ _ who would not climb _ such a lad -- der as this?
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
  d8 |
  b[ e] d c |
  b4 fis8 g |
  fis[ g] a a |
  a4 b8( c) |
  
  b[ e] d c |
  b4 g8 g |
  fis[ g] a b |
  a4 g8( a) |
  b4 b8 a |
  
  a4 d8 d |
  c4 a8 a |
  \slurDotted
  a4 g8( g) |
  g4 d'8 c |
  b[ gis] a c |
  
  b4 d8 c |
  b4 \bar "||"
  d8 d |
  b4 b8 b |
  c4 cis8 cis |
  d4 a8 a |
  
  fis[ a] g fis |
  g4 g8 g |
  g4 g8 g |
  fis[ g] a b |
  
  a4 d8 d |
  d4 d8 c |
  b[ d] e c |
  b4 d8 c |
  b4. \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  d8 |
  g[ c] b a |
  g4 fis8 e |
  d[ e] fis g |
  fis4 b,8( b) |
  
  e8[ c] b a |
  g4 e'8 e |
  d[ e] fis g |
  fis4 g8( g) |
  b,4 b8 cis |
  
  d4 d8 d |
  a'4 g8 g |
  \slurDotted
  fis4 g8( e) |
  b4 b8 c |
  d4 d8 d |
  
  e4 b8 c |
  g4 \bar "||"
  d'8 d |
  g4 f8 f |
  e4 ees8 ees |
  d4 c8 c |
  
  b4 b8 b |
  e[ c] b a |
  g4 e'8 e |
  d[ e] fis g |
  
  fis4 g8 a |
  b4 b,8 c |
  d4 d8 d |
  e4 b8 c |
  g4. \bar "|."
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
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.9))} \lyricsto "altos" \altoWords
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
    \tempo 4 = 105
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
