\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Carol of the Shepherds"}}
  poet = \markup\oldStyleNum"English by Eda Lou Walton (1894–1961)"
  composer = \markup\oldStyleNum"17th Century Bohemian Carol"
  tagline = ""
}
\paper {
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
  first-page-number = #054
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
  \key g \major
  \time 3/4
  \autoBeamOff
}

sopMusic = \relative c'' {
  d4 d8[ b] e[ c] |
  d4 d8[ b] e[ c] |
  d4 b8[ d] a[ b] g2 b4\rest | \break
  
  d4 d8[ b] e[ c] |
  d4 d8[ b] e[ c] |
  d4 b8[ d] a[ b] g2 b4\rest | \break
  
  g4 b8\noBeam g\noBeam b\noBeam d\noBeam |
  g,4 b8\noBeam g\noBeam a\noBeam d,\noBeam |
  g4 b8\noBeam g\noBeam b\noBeam d\noBeam | \break
  
  g,4 b8\noBeam g\noBeam a\noBeam d,\noBeam |
  d'4 b8[ d] a[ b] |
  \partial 2 g2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c'' {
  b4 b8[ g] c[ a] |
  b4 b8[ g] c[ a] |
  b4 g fis g2 s4 |
  
  b4 b8[ g] c[ a] |
  b4 b8[ g] c[ a] |
  b4 g fis g2 s4 |
  
  g4 b8\noBeam g\noBeam b\noBeam d\noBeam |
  g,4 b8\noBeam g\noBeam a\noBeam d,\noBeam |
  g4 b8\noBeam g\noBeam b\noBeam d\noBeam |
  
  g,4 b8\noBeam g\noBeam a\noBeam d,\noBeam |
  g4 g fis |
  d2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  Come, all __ ye __ shep -- herds and
__
  be not __ dis -- mayed,
  Seek where the low -- ly __ sweet ba -- by __ is __ laid;
  Here in a man -- ger,
  far from all dan -- ger,
  Sleep -- ing be -- hold Him,
  Warm arms en -- fold Him
  In Christ -- mas __ joy.
}
altoWordsII = \lyricmode {
  \dropLyricsV
  \set stanza = #"2. "
  As we __ were watch -- ing __ our
__
  flocks where they lay,
  Shown a __ great glo -- ry __ as __ bright as __ the __ day.
  Glad bells were ring -- ing, sweet voi -- ces sing -- ing,
  Through heav’n’s blue por -- tals, “Good will to mor -- tals;”
  Christ -- mas __ is __ come.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  Now we __ have found Him in __ Beth -- le -- hem stall,
  Sing the __ glad ti -- dings, oh, __ sing them to __ all!
  Shep -- herds a -- dore Him, wise men be -- fore Him
  Lay down their dow -- er, In glitt -- ’ring show -- er,
  Christ -- mas __ is __ come.
}
altoWordsIV = \lyricmode {

}

tenorMusic = \relative c' {
  g4 g g |
  g g g |
  g g d' |
  g,2 s4 |
  
  g g g |
  g g g |
  g g d' |
  g, 2 s4 |
  
  g4 b8\noBeam g\noBeam b\noBeam d\noBeam |
  g,4 b8\noBeam g\noBeam a\noBeam d,\noBeam |
  g4 b8\noBeam g\noBeam b\noBeam d\noBeam |
  
  g,4 b8\noBeam g\noBeam a\noBeam d\noBeam |
  b4 d c |
  b2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c' {
  g4 g g |
  g g g |
  g g fis |
  g2 d4\rest |
  
  g g g |
  g g g |
  g g d |
  g2 d4\rest |
  
  g4 b8\noBeam g\noBeam b\noBeam d\noBeam |
  g,4 b8\noBeam g\noBeam a\noBeam d,\noBeam |
  g4 b8\noBeam g\noBeam b\noBeam d\noBeam |
  
  g,4 b8\noBeam g\noBeam a\noBeam d,\noBeam |
  d4 d d |
  g2 \bar "|."
}
bassWords = \lyricmode {

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
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWords
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

