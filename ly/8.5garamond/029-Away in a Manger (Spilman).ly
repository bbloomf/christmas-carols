\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Away In A Manger"}}
  poet = \markup\oldStyleNum"Anonymous"
  composer = \markup\oldStyleNum"Jonathan E. Spilman (1812–1896)"
  tagline = \markup { "from" \concat{\italic "Christmas Carols and Hymns for School and Choir" \oldStyleNum", 1910"}}
}
\paper {
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #029
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
  \key a \major
  \time 3/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \partial 4 e4^\p |
  a4 a cis8[ b] |
  a4 a e |
  fis a fis |
  e2 e4 |
  a a b |
  
  cis cis e |
  e cis a |
  b2 e,4 |
  a a cis8[ b] |
  a4 a e |
  fis d' fis, |
  
  e2 e4 |
  a a b |
  cis e\fermata d |
  e, e gis |
  a2\fermata gis8[^\markup\italic"piu mosso" a] |
  b4 b e |
  b b gis |
  
  b a fis |
  e2 gis8[ a] |
  b4 b e |
  b b gis |
  a8[ gis]^\markup\italic"poco rit." a[ b] cis[ dis] |
  << e2 {s4. s8^\f} >> fis4\fermata |
  << e4-> {s16 s8.^\markup\italic"a tempo"}>> cis4 cis8[ b] |
  
  a4 a e |
  fis d' fis, |
  e2 e4^\p |
  a a b |
  cis^\pp e\fermata d |
  e, e gis |
  \partial 2 a2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  e4 |
  e e e |
  e e cis |
  d fis d |
  cis2 cis4 |
  cis cis e |
  
  e e e |
  e e dis |
  e2 e4 |
  e e e |
  e e cis |
  d fis d |
  
  cis2 cis4 |
  cis e e |
  e e fis |
  e cis d |
  cis2 e8[ fis] |
  gis4 gis gis |
  gis gis e |
  
  dis dis dis |
  e2 e8[ fis] |
  gis4 gis gis |
  gis gis e |
  fis8[ e] fis[ gis] a4 |
  gis2 gis4 |
  a e e |
  
  e e cis |
  d fis d |
  cis2 cis4 |
  cis e e |
  e a fis |
  e cis d |
  cis2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsVII
  \set stanza = #"1. "
  A -- way in a man -- ger,
  No crib for His bed,
  The lit -- tle Lord Je -- sus
  Laid down His sweet head:
  The stars in the heav -- ens
  Look’d down where He lay,
  The lit -- tle Lord Je -- sus
  A -- sleep in the hay.
  
  The cat -- tle are low -- ing,
  The poor ba -- by wakes,
  But lit -- tle Lord Je -- sus
  No cry -- ing He __ makes;
  I love Thee, Lord Je -- sus,
  Look down from the sky,
  And stay by my cra -- dle
  Till mor -- ning is nigh.
}
altoWordsII = \lyricmode {
  \dropLyricsVII
  \set stanza = #"2. "
  Be near me, Lord Je -- sus,
  I ask Thee to stay
  Close by me for -- ev -- er
  And love me, I pray:
  Bless all the dear chil -- dren
  In Thy ten -- der care,
  And take us to heav -- en
  To live with Thee there.
  
  A -- way in a man -- ger,
  No crib for His bed,
  The lit -- tle Lord Je -- sus
  Laid down His sweet head:
  The stars in the heav -- ens
  Look’d down where He lay,
  The lit -- tle Lord Je -- sus
  A -- sleep in the hay.
}
altoWordsIII = \lyricmode {
}
altoWordsIV = \lyricmode {
}

tenorMusic = \relative c {
  e4_\p |
  cis' cis e8[ d] |
  cis4 cis a |
  a a a |
  a2 a4 |
  a a gis |
  
  a a cis |
  cis a a |
  gis2 e4 |
  cis' cis e8[ d] |
  cis4 cis a |
  a a a |
  
  a2 a4 |
  a a gis |
  a cis b |
  cis a b |
  a2 b4\rest |
  a1*3/4\rest |
  a1*3/4\rest |
  
  a1*3/4\rest |
  a2\rest b4\rest |
  a1*3/4\rest |
  a1*3/4\rest |
  a1*3/4\rest |
  << a2\rest {s4. s8_\f} >> d4 |
  cis-> a e'8[ d] |
  
  cis4 cis a |
  a a a |
  << a2 {s4. s8_\p} >> a4 |
  a a gis |
  a_\pp cis b |
  cis a b |
  a2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  e4 |
  a, a a |
  a a a |
  d d d |
  a2 a4 |
  a cis e |
  
  a a a |
  a a fis |
  e2 e4 |
  a, a a |
  a a a |
  d d d |
  
  a2 a4 |
  a cis e |
  a a\fermata d, |
  e e e |
  a,2\fermata e'4 |
  e e e |
  e e e |
  
  b b b |
  e2 e4 |
  e e e |
  e e e |
  b b b |
  e2 e4\fermata |
  a,4 a a |
  
  a a a |
  d d d |
  a2 a4 |
  a cis e |
  a a\fermata d, |
  e e e |
  a,2 \bar "|."
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
    \new Lyrics \with { alignAboveContext = #"women" } \lyricsto "sopranos" \sopWords
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
      \override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/2)
      \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/2)
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

