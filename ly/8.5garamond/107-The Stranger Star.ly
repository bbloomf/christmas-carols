\version "2.24.0"
\include "util.ly"
\version "2.24.0"
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Stranger Star"}}
  poet = \markup\oldStyleNum"Cecil Frances Alexander (1818–1895)"
  composer = \markup\oldStyleNum"J. A. Shultze, 1780"
  tagline = \markup \concat{ "from " \italic "Favorite Songs and Hymns for School and Home" \oldStyleNum", 1899, via "\italic"books.google.com"}
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
  first-page-number = #107
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
global = {
  \key e \major
  \time 2/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  gis'4. fis8 |
  e4. dis8 |
  e[ fis] gis[ a] |
  gis4 fis |
  gis4. a8 |
  b4. a8 |
  gis4 e |
  fis2 |
  
  gis4. fis8 |
  e4. dis8 |
  e[ fis] gis[ a] |
  gis4 fis |
  gis4. a8 |
  b4. a8 |
  gis[ fis] e[ dis] |
  e2 |
  
  cis'4. dis8 |
  e4. cis8 |
  b8[ gis] fis[ e] |
  gis4 fis |
  b4. gis8 |
  cis4. dis8 |
  e[ cis] b[ gis] |
  fis2 |
  
  gis4. fis8 |
  e4. dis8 |
  e[ fis] gis[ a] |
  gis4 fis |
  gis4. a8 |
  b4 cis8[ e]\fermata |
  gis,4 fis |
  e2\fermata \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  e4. b8 |
  b4. b8 |
  b[ dis] e4 |
  e dis |
  e4. dis8 |
  e4. e8 |
  e4 e |
  dis2 |
  
  e4. b8 |
  b4. b8 |
  b[ dis] e4 |
  e dis |
  e4. dis8 |
  e4. e8 |
  e[ dis] b4 |
  b2 |
  
  e4. e8 |
  e4. e8 |
  e4 cis |
  e dis |
  e4. fis8 |
  e4. fis8 |
  e4 e |
  dis2 |
  
  e4. b8 |
  b4. b8 |
  b[ dis] e4 |
  e dis |
  e4. dis8 |
  e4 e\fermata |
  e dis |
  e2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsVII
  \set stanza = #"1. "
  Saw ye nev -- er in the twi -- light,
  When the sun had left the skies,
  Up in heav’n the clear stars shin -- ing
  Thro’ the gloom like lov -- ing eyes?
  So of old the wise men watch -- ing,
  Saw a blaz -- ing stran -- ger __ star,
  And they knew the King was giv -- en,
  And they fol -- lowed it from far.
}
altoWordsII = \lyricmode {
  \dropLyricsVII
%\markup\italic
  \set stanza = #"2. "
  Heard ye nev -- er of the sto -- ry,
  How they crossed the des -- ert wild,
  Jour -- neyed on by plain and moun -- tain,
  Till they found the Ho -- ly __ Child?
  How they o -- pen’d all their trea -- sure,
  Kneel -- ing to that In -- fant King,
  Gave the gold and fra -- grant in -- cense,
  Gave the myrrh in __ of -- fer -- ing?
}
altoWordsIII = \lyricmode {
  \dropLyricsVII
  \set stanza = #"3. "
  Know ye not that low -- ly Ba -- by
  Was the bright and Morn -- ing Star,
  He who came to light the Gen -- tiles,
  And the dark -- ened isles a -- far?
  And we too may seek His __ cra -- dle,
  There our hearts’ best trea -- sures bring,
  Love and faith and true de -- vo -- tion,
  For our Sav -- ior, God, and King.
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
  b4. a8 |
  gis4. a8 |
  b4 b8[ cis] |
  b4 b |
  b4. b8 |
  b4. cis8 |
  b4 b |
  b2 |
  
  b4. a8 |
  gis4. a8 |
  b4 b8[ cis] |
  b4 b |
  b4. b8 |
  b4. cis8 |
  b8[ a] gis[ a] |
  gis2 |
  
  a4. b8 |
  cis4. a8 |
  gis8[ b] ais4 |
  b4 b8[ a] |
  b4. bis8 |
  cis4. bis8 |
  cis4 gis8[ b] |
  b2 |
  
  b4. a8 |
  gis4. a8 |
  b4 b8[ cis] |
  b4 b |
  b4. b8 |
  b4 cis |
  b4 a |
  gis2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  \mergeDifferentlyHeadedOn
  e4. e8 |
  e4. fis8 |
  gis[ fis] e[ a,] |
  b4 b |
  e4. fis8 |
  gis4. a8 |
  b4 gis8[ e] |
  b2 |
  
  e4. e8 |
  e4. fis8 |
  gis[ fis] e[ a,] |
  b4 b |
  e4. fis8 |
  gis4. a8 |
  b4 b, |
  e2 |
  
  a,4. a8 |
  a4. cis8 |
  e4 cis |
  b b |
  gis'4. gis8 |
  gis4. gis8 |
  a4 e8[ gis] |
  b,2 |
  
  e4. e8 |
  e4. fis8 |
  gis[ fis] e[ a,] |
  b4 b |
  e4. fis8 |
  gis4 a\fermata |
  b b, |
  e2\fermata \bar "|."
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
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "sopranos" \altoWords
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

