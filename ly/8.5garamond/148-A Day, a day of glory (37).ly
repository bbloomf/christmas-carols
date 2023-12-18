\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"A Day, a Day of Glory"}}
  poet = \markup\oldStyleNum"John Mason Neale (1818–1866)"
  composer = \markup\oldStyleNum"Old French"
  arranger = \markup\oldStyleNum"Arranged by Dr. Charles Wood (1866–1926)"
  tagline = \markup\concat { "from " \italic "The Cowley Carol Book" \oldStyleNum", 1919"}
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
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
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #148
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
  \key c \major
  \time 3/4
  \autoBeamOff
}

sopMusic = \relative c' {
  \partial 4 a'4 |
  e'4. e8 b c |
  d4 d4. a8 |
  c4. d8 b c |
  \partial 2 a2 | \break
  
  \partial 4 a4 |
  e'4. e8 b c |
  d4 d4. a8 |
  c4. d8 b c |
  \partial 2 a2 | \break
  
  \partial 4 a4 |
  b4. gis8 a b |
  c4 c b8\rest a |
  d4. c8 b a | 
  
  \partial 2 gis2 | \break
  \partial 4 a4 |
  b4. gis8 a b |
  c4 c b8\rest a |
  b4. a8 a gis |
  \partial 2 a2\fermata \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c'' {
  a4 |
  a4. a8 b a |
  a4 a4. a8 |
  a4 a a8 gis |
  a2
  
  a4 |
  a4. a8 b a |
  a4 a4. a8 |
  a4 a a8 gis |
  a2 |
  
  e4 |
  e4. e8 e e |
  e4 e s8 e |
  d4. d8 f f |
  
  e2 e4 |
  e4. e8 fis gis |
  a4 a s8 e |
  f4. f8 e e |
  c2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  A day, a day of glo -- ry!
    A day that ends our woe!
  A day that tells of tri -- umph
    A -- gainst our van -- quish’d foe!
  Yield, sum -- mer’s bright -- est sun -- rise,
    To this De -- cem -- ber morn:
  Lift up your gates, ye Prin -- ces,
    And let the Child be born!
}
altoWordsII = \lyricmode {
  \dropLyricsV
  \set stanza = #"2. "
  With \markup\italic Glo -- \markup\italic ria \markup\italic in \markup\italic ex -- \markup\italic cel -- \markup\italic sis
    Arch -- an -- gels tell their mirth:
  With \markup\italic Ky -- \markup\italic ri -- \markup\italic e \markup\italic e -- \markup\italic lei -- \markup\italic son
    Men an -- swer up -- on earth:
  And an -- gels swell the tri -- umph,
    And mor -- tals raise the horn,
  
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  He comes, His throne the man -- ger;
    He comes, His shrine the stall;
  The ox and ass His cour -- tiers,
    Who made and gov -- erns all:
  The “House of Bread” His birth -- place,
    The Prince of wine and corn:
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = #"4. "
  Then bar the gates, that hence -- forth
    None thus may pas -- sage win,
  Be -- cause the Prince of Is -- rael
    A -- lone hath en -- ter’d in:
  The earth, the sky, the o -- cean
    His glo -- rious way a -- dorn:
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
  c4 |
  e4. c8 e e |
  f4 f4. d8 |
  e4 f e8 e |
  c2
  
  c4 |
  e4. c8 e e |
  f4 f4. d8 |
  e4 f e8 e |
  c2
  
  c4 |
  gis4. b8 a gis |
  a4 a s8 a |
  a4. a8 d c |
  
  b2 c4 |
  b4. b8 e e |
  e4 e s8 e |
  d4. d8 b b |
  a2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c' {
  a4 |
  c4. a8 gis a |
  d,4 d4. f8 |
  a4 d, e8 e |
  a2
  
  a4 |
  c4. a8 gis a |
  d,4 d4. f8 |
  a4 d, e8 e |
  a2
  
  a,4 |
  e'4. d8 c b |
  a4 a d8\rest c |
  f4. f8 d d |
  
  e2 a4 |
  gis4. e8 e d |
  c4 a d8\rest c |
  d4. d8 e e |
  a,2\fermata \bar "|."
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
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWords
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

