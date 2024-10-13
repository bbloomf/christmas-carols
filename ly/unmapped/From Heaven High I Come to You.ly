\version "2.14.2"
\include "util.ly"
\header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"From Heaven High I Come to You"}}
    poet = \markup\oldStyleNum"Martin Luther (1483–1546)"
    meter = \markup\oldStyleNum"Translated by Catherine Winkworth (1827–1878)"
    composer = \markup\oldStyleNum"Old German Melody Attributed to Martin Luther"
    arranger = \markup\oldStyleNum"Adapted by J.S. Bach (1685–1750)"
    tagline = \markup \concat{ "from " \italic "The English Hymnal" \oldStyleNum", 1906"}
  }\paper {
  print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
%{IF_LESSER
  markup-system-spacing #'stretchability = 50
  top-markup-spacing #'stretchability = 30
  last-bottom-spacing #'stretchability = 60
%}%END_IF_LESSER
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #076
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
%IF_NOT_LESSER
global = {
  \key c \major
  \time 4/4
  \autoBeamOff
  \mergeDifferentlyHeadedOn
}

sopMusic = \relative c'' {
  \tempo \markup\italic"Very slow and dignified" 4 = 46
  \partial 4 c4 |
  b a b g |
  a b c\fermata \bar "||"
  
  c |
  c g g e8[ f] |
  g4 f e\fermata \bar"||"
  
  e |
  a a g8[ a] b4 |
  c8[ b] a4 g\fermata
  
  c4 |
  b a g a8[ g] |
  f[ e] d4 c\fermata \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  g'4 |
  g8[ fis] e4 d e8[ d] |
  c4 d e |
  
  e8[ f] |
  g[ f] e4 d c |
  c d c |
  
  c8[ d] |
  e4 d d8[ fis] g4 |
  g g8[ fis] g4 |
  
  e8[ d] |
  d[ e] e[ d] d[ c] c4 |
  c8[ a] b4 c \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  From heav -- en high I come to you,
  To bring you ti -- dings, strange and true.
  Glad ti -- dings of great joy I bring,
  Where of I now will say and sing.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  To you this night is born a Child
  Of Ma -- ry, chos -- en Moth -- er mild;
  This lit -- tle Child, of low -- ly birth,
  Shall be the joy of all the earth.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  Glo -- ry to God in high -- est Heav’n,
  Who un -- to us His Son hath giv’n!
  While an -- gels sing with pi -- ous mirth
  A glad New Year to all the earth.
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  e4 |
  d c b c8[ b] |
  a[ g] f4 g |
  
  g4 |
  c c b bes8[ a] |
  g4 g g |
  
  g |
  g fis g8[ c] b[ a] |
  g[ b] e[ d] b4 |
  
  a4 |
  g g8[ f] f[ e] a[ b] |
  c4 g8[ f] e4 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  c4 |
  g'4. fis8 g[ f] e4 |
  f8[ e] d4 c |
  
  c8[ d] |
  e4 e8[ f] g4 g8[ f] |
  e[ d] c[ b] c4 |
  
  c |
  cis d8[ c] b[ a] g4 |
  e'8[ d] c[ d] g4 |
  
  g8[ fis] |
  g[ e] c[ d] b[ c] f[ g] |
  a[ f] g[ g,] c4 \bar "|."
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
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "sopranos" \altoWords
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
    \tempo 4 = 46
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
