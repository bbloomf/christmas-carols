\version "2.14.2"
\include "util.ly"
\header {
  tagline = ""%\markup { "from" \italic {ChristmasCarolMusic.org}}

    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Away In A Manger"}}
    poet = \markup\oldStyleNum"Anonymous"
    composer = \markup\oldStyleNum"James Ramsey Murray (1841–1905)"
    tagline = ""%\markup { "from" \italic {ChristmasCarolMusic.org}}
  }
\paper {
  print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 80))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -10)
       (stretchability . 100))
  markup-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
  last-bottom-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
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
  first-page-number = #028
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
#(set-global-staff-size 13.6) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 13.6 20))) }
%8.5x11 #(set-global-staff-size 17.8) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 17.8 20))) }
%{IF_LESSER
#(set-global-staff-size 15) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 15 20))) }
%}%END_IF_LESSER
global = {
  \key f \major
  \time 3/4
  \autoBeamOff
}

sopMusic = \relative c'' {
  \partial 4 c4 |
  c4. bes8 a4 |
  a g f |
  f e d |
  c2 c4 |
  c4. d8 c4 |
  
  c g' e |
  d c f |
  a2 c4 |
  c4. bes8 a4 |
  \slurDotted a g f |
  
  f e d |
  c2 c4 |
  bes'4. a8 g4 |
  a g f |
  g d e |
  \partial 2 f2 \bar "|."
%{IF_LESSER
\pageBreak
%}%END_IF_LESSER
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  f4 |
  f4. g8 f4 |
  f e d |
  d c bes |
  a2 a4 |
  bes4. bes8 bes4 |
  
  bes bes bes |
  bes a c |
  f2 a8[ g] |
  f4. g8 f4 |
  f e d |
  
  d c bes |
  a2 c4 |
  e4. f8 g4 |
  f e d |
  d bes bes8[ c] |
  a2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIV
  \set stanza = #"1. "
  A -- way in a man -- ger,
  No crib for His bed,
  The lit -- tle Lord Je -- sus
  Laid down His sweet head:
  The stars in the heav -- ens
  Look’d down where He lay,
  The lit -- tle Lord Je -- sus
  A -- sleep in the hay.
}
altoWordsII = \lyricmode {
  \dropLyricsIV
  \set stanza = #"2. "
  The cat -- tle are low -- ing,
  The poor ba -- by wakes,
  But lit -- tle Lord Je -- sus
  No cry -- ing He makes;
  I love Thee, Lord Je -- sus,
  Look down from the sky,
  And stay by my cra -- dle
  Till mor -- ning is nigh.
}
altoWordsIII = \lyricmode {
  \dropLyricsIV
  \set stanza = #"3. "
  Be near me, Lord Je -- sus,
  I ask Thee to stay
  Close by me for -- ev -- er
  And love me, I pray:
  Bless all the dear chil -- dren
  In Thy ten -- der care,
  And take us to heav -- en
  To live with Thee there.
}
altoWordsIV = \lyricmode {
}

tenorMusic = \relative c {
  a'4 |
  a4. bes8 c4 |
  c bes a |
  bes g f |
  f2 f4 |
  e4. f8 e4 |
  
  e e g |
  f f a |
  c2 c8[ bes] |
  a4. bes8 c4 |
  c bes a |
  
  bes g f |
  f2 c'4 |
  c4. c8 c4 |
  c bes a |
  bes g g |
  f2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  f4 |
  f4. e8 f4 |
  f,4 c' d |
  bes c bes8[ c] |
  f2 f4 |
  c4. bes8 c4 |
  
  c c c |
  bes f' f |
  f2 f4 |
  f4. e8 f4 |
  f, c' d |
  
  bes c bes8[ c] |
  f2 c4 |
  c4. d8 e4 |
  f c d |
  bes bes c4 |
  f,2 \bar "|."
}
bassWords = \lyricmode {

}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" } \lyricsto "sopranos" \sopWords
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
    %#(layout-set-staff-size 13)
    #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 14 20)))
    %8.5x11 #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 17.8 20)))
%{IF_LESSER
#(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 15 20)))
%}%END_IF_LESSER
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 2)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 2)
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
    \tempo 4 = 100
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
