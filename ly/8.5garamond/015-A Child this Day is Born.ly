\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"A Child this day is born"}}
  poet = \markup\oldStyleNum"Traditional"
  composer = \markup\oldStyleNum"Traditional"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
       
  last-bottom-spacing =
    #'((basic-distance . -10)
       (minimum-distance . -10)
       (padding . 0)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #015
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
  \key d \major
  \time 4/4
  \autoBeamOff
  \slurDashed
}

sopMusic = \relative c' {
  \partial 4 a'4 |
  a d cis a8[ b] |
  cis2. e4 |
  d4 cis d8[ cis] b4 |
  
  a2. \bar"" \break
  a4 |
  a d, b' d |
  a4.( b8) a4 g |
  fis d' fis, e |
  \partial 2. d2. \bar "||" \break
  
  
  \partial 4 a'4 |
  a d cis a8[ b] |
  cis2. e4 |
  d4 cis d8[ cis] b4 |
  
  a2. \bar"" \break
  a4 |
  a d, b' d |
  \slurSolid
  a4.( b8 a4) g |
  fis d' fis, e |
  \partial 2. d2. \bar ":|."
  
  
  
  
  
  
  \slurDashed
  \partial 4 a'4 |
  a d cis a8[ b] |
  cis2. e4 |
  d4 cis d8[ cis] b4 |
  
  a2. \bar"" \break
  a4 |
  a d, b' d |
  a4. b8 a4 g |
  fis d' fis, e |
  \partial 2. d2. \bar "||" \break
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  fis4 |
  a a a fis |
  a2. a4 |
  gis a a gis |
  
  a2. e4 |
  d d d d |
  d4.( d8) d4 cis |
  d d d cis |
  d2. \bar "||"

  
  fis4 |
  a a a fis |
  a2. a4 |
  gis a a gis |
  
  a2. e4 |
  d d d d |
  \slurSolid
  d4.( d8)~ d4 cis |
  d d d cis |
  d2.
  
  
  
  
  
  \slurDashed
  fis4 |
  a a a fis |
  a2. a4 |
  gis a a gis |
  
  a2. e4 |
  d d d d |
  d4. d8 d4 cis |
  d d d cis |
  d2. \bar "||"
}
altoWords = \lyricmode {
  \dropLyricsXII
  \set stanza = #"1. "
  A Child this day is born,
  A Child of high re -- nown;
  Most wor -- thy of a scep -- tre,
  A scep -- tre and a crown.
  
  Glad tid -- ings to all men,
  Glad tid -- ings sing we may,
  Be -- cause the King of kings __
  Was born on Christ -- mas Day.
  
  
  \set stanza = #"4. "
  They praised the Lord our God
  And our ce -- le -- stial King:
  All glo -- ry be in
\set ignoreMelismata = ##t
  Pa -- ra -- dise,
\unset ignoreMelismata
  This heav’n -- ly host do sing.
}
altoWordsII = \lyricmode {
%\markup\italic
  
}
altoWordsIII = \lyricmode {
  \dropLyricsXII
  \set stanza = #"2. "
  These tid -- ings shep -- herds heard
  Whilst watch -- ing o’er their fold,
  ’Twas by an An -- gel
\set ignoreMelismata = ##t
  un -- to them
\unset ignoreMelismata
  That night re -- vealed and told.
  
  \repeat unfold24 \skip1
  \set stanza = #"5. "
  All glo -- ry be to God,
  That sit -- teth still on high,
  With prais -- es and with
\set ignoreMelismata = ##t
  tri -- umph great,
\unset ignoreMelismata
  And joy -- ful mel -- o -- dy.
}
altoWordsIV = \lyricmode {
  \dropLyricsXII
  \set stanza = #"3. "
  Then was there with the~An -- gel
  An host in -- con -- ti -- nent
  Of heav -- en -- ly bright sol -- diers,
  All from the high -- est sent.
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  d4 |
  d fis e d |
  e2. cis4 |
  d e fis d |
  
  cis2. cis4 |
  d a g b |
  d4.( g,8) a4 a |
  a b a g |
  fis2. \bar "||"
  
  
  d'4 |
  d fis e d |
  e2. cis4 |
  d e fis d |
  
  cis2. cis4 |
  d a g b |
  \slurSolid d4.( g,8 a4) a |
  a b a g |
  fis2.
  
  
  
  
  
  \slurDashed
  d'4 |
  d fis e d |
  e2. cis4 |
  d e fis d |
  
  cis2. cis4 |
  d a g b |
  d4. g,8 a4 a |
  a b a g |
  fis2. \bar "||"
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  d4 |
  fis d a' d |
  a2. cis4 |
  b a d, e |
  
  a2. g4 |
  fis fis g g |
  fis4.( g8) fis4 e |
  d g, a a |
  d2. \bar "||"

  
  d4 |
  fis d a' d |
  a2. cis4 |
  b a d, e |
  
  a2. g4 |
  fis fis g g |
  \slurSolid
  fis4.( g8 fis4) e |
  d g, a a |
  d2.
  
  
  
  
  
  \slurDashed
  d4 |
  fis d a' d |
  a2. cis4 |
  b a d, e |
  
  a2. g4 |
  fis fis g g |
  fis4. g8 fis4 e |
  d g, a a |
  d2. \bar "||"
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
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.9))} \lyricsto "altos" \altoWords
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

