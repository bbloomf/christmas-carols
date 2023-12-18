\version "2.24.0"
\include "util.ly"
\header {
  tagline = ""%\markup { "from" \italic {ChristmasCarolMusic.org}}
}
\paper {
  print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
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
  markup-system-spacing.stretchability = 50
  top-markup-spacing.stretchability = 30
  last-bottom-spacing.stretchability = 60
%}%END_IF_LESSER
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #028
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
#(set-global-staff-size 17.8) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 17.8 20))) }
%{IF_LESSER
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
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
  \slurDashed a g f |
  
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
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
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
    %#(layout-set-staff-size 13)
    #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20)))
    #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 17.8 20)))
%{IF_LESSER
#(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20)))
%}%END_IF_LESSER
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
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Away In A Manger"}}
    poet = \markup\oldStyleNum"Anonymous"
    composer = \markup\oldStyleNum"James Ramsey Murray (1841–1905)"
    tagline = ""%\markup { "from" \italic {ChristmasCarolMusic.org}}
  }
}





























global = {
  \key f \major
  \time 3/4
  \autoBeamOff
}

sopMusic = \relative c' {
  \partial 4 c4 |
  f f g8[ a] |
  f4 f a8[ bes] |
  c4 c d |
  bes2 g8[ a] |
  bes4 bes c |
  
  a a f8[ a] |
  g4 d f |
  e2 \bar"" c4 |
  f f g8[ a] |
  f4 f a8[ bes] |
  
  c4 c d |
  bes2 g8[ a] |
  bes4 bes c |
  a4 a f8[ a] |
  g4 d e |
  f2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  c4 |
  c d e |
  d c c |
  f4 f f |
  g2 d4 |
  g g e |
  f f d |
  
  d d c 
  c2 c4 |
  c d e |
  d c c |
  f4 f f |
  g2 d4 |
  g g e |
  f f d |
  d d bes |
  c2 \bar"|."
}
altoWords = \lyricmode {
  \dropLyricsIV
  \set stanza = #"1. "
  A -- way in a  man -- ger,
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
  I __ ask Thee to stay
  Close by me for -- ev -- er
  And love me, I pray:
  Bless all the dear chil -- dren
  In __ Thy ten -- der care,
  And take us to heav -- en
  To live with Thee there.
}
altoWordsIV = \lyricmode {
}

tenorMusic = \relative c' {
  c4 |
  a a c |
  a a a8[ g] |
  a4 a bes |
  d2 bes8[ c] |
  d4 d c |
  a a a8[ f] |
  
  bes4 bes a |
  g2 c4 |
  a a c |
  a a a8[ g] |
  a4 a bes |
  d2 bes8[ c] |
  d4 d c |
  a a a8[ f] |
  bes4 bes g |
  a2 \bar"|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  c4 |
  f d c |
  d f f8[ e] |
  f4 f bes, |
  g'2 g4 |
  g g c, |
  d d d |
  
  g g c, |
  c2 c4 |
  f d c |
  d f f8[ e] |
  f4 f bes, |
  g'2 g4 |
  g g c, |
  d d d |
  g g c, |
  f2 \bar"|."
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
    %#(layout-set-staff-size 13)
    #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20)))
    #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 17.8 20)))
%{IF_LESSER
#(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20)))
%}%END_IF_LESSER
    \context {
      \Score
      \override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/2)
      \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/2)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
    }
\context {\Lyrics \override LyricText.font-size = #0.7 }
%6.14g    \context {\Lyrics \override LyricText.font-size = #0.7 }
%6x9 \context {\Lyrics\override LyricText.font-size = #1.2 }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Away In A Manger"}}
    poet = \markup\oldStyleNum"Anonymous"
    composer = \markup\oldStyleNum"William Kirkpatrick (1838–1921)"
  }
}

