\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"It Came Upon the Midnight Clear"}}
  poet = \markup\oldStyleNum"Edmund H. Sears (1810–1876)"
  composer = \markup\oldStyleNum"Richard S. Willis (1819–1900)"
  tagline = \markup { "from" \concat{\italic "Christmas Carols and Hymns for School and Choir" \oldStyleNum", 1910"}}
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
  first-page-number = #020
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
  \key bes \major
  \time 6/8
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \partial 8 f8 |
  d'4 a8 c[ bes] g8 |
  f4 g8 f4 f8 |
  g[ a] bes bes[ c] d |
  c4.~ c4 \bar""\break
  
  f,8 |
  d'4 a8 c[ bes] g8 |
  f4 g8 f4 f8 |
  g4 g8 a[ g] f |
  bes4.~bes4 | \bar""\break
  
  d8 |
  d4 d,8 d[ e] fis |
  g4 a8 bes4 d8 |
  c[ bes] a g[ a] g |
  f4.~f4 \bar""\break
  
  f8 |
  d'4 a8 c[ bes] g8 |
  f4 g8 f4 f8 |
  g4 g8 a[ g] f |
  bes4.~bes4 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d8 |
  d4 d8 ees4 ees8 |
  d4 ees8 d4 d8 |
  ees4 ees8 e4 e8 |
  f4.~f4 |
  
  d8 |
  d4 d8 ees4 ees8 |
  d4 ees8 d4 f8 |
  ees4 ees8 ees4 ees8 |
  d4.~ d4 |
  
  d8 |
  d4 d8 d[ e] d |
  d4 d8 d4 d8 |
  f4 f8 e4 e8 |
  f4.~ f4 |
  
  ees8 |
  d4 d8 ees4 ees8 |
  d4 ees8 d4 f8 |
  ees4 ees8 ees4 ees8 |
  d4.~ d4 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsVI
  \set stanza = #"1. "
  It came up -- on __ the mid -- night clear,
  That glo -- rious song of old, __
  From an -- gels bend -- ing near the earth
  To touch their harps of gold: __
  “Peace on the earth, good -- will to men
  From heav’n’s all gra -- cious King.”
__
  The world in sol -- emn still -- ness lay
  To hear the an -- gels sing. __
}
altoWordsII = \lyricmode {
  \dropLyricsVI
  \set stanza = #"2. "
  Still through the clo -- ven skies they come,
  With peace -- ful wings un -- furl’d;
__
  And still their heav’n -- ly mu -- sic floats
  O’er all the wea -- ry world:
__
  A -- bove its sad __ and low -- ly plains
  They bend __ on hov -- ’ring wing, __
  And ev -- er o’er __ its Ba -- bel sounds
  The bless -- ed an -- gels sing. __
}
altoWordsIII = \lyricmode {
  \dropLyricsVI
  \set stanza = #"3. "
  O ye, be -- neath life’s crush -- ing load,
  Whose forms are bend -- ing low, __
  Who toil a -- long the climb -- ing way
  With pain -- ful steps and slow! __
  Look now, for glad __ and gold -- en hours
  Come swift -- ly on __ the wing; __
  O rest be -- side __ the wea -- ry road
  And hear the an -- gels sing. __
}
altoWordsIV = \lyricmode {
  \dropLyricsVI
  \set stanza = #"4. "
  For lo! the days are hast -- ’ning on,
  By pro -- phet bards fore -- told, __
  When with the ev -- er -- cir -- cling years
  Comes round the age of gold; __
  When Peace shall o -- ver all the earth
  Its an -- cient splen -- dors fling, __
  And the whole world send back the song
  Which now the an -- gels sing. __
}

tenorMusic = \relative c {
  f8 |
  f4 fis8 g4 a8 |
  bes4 f8 f4 bes8 |
  bes4 bes8 bes4 bes8 |
  a4.~ a4 |
  
  bes8 |
  f4 fis8 g4 a8 |
  bes4 f8 f4 bes8 |
  bes4 c8 c[ bes] a |
  bes4.~ bes4 |
  
  d,8 |
  d4 fis8 fis[ g] a |
  g4 fis8 g4 g8 |
  a[ d] c bes[ c] bes |
  a4.~ a4 |
  
  a8 |
  f4 fis8 g4 a8 |
  bes4 f8 f4 bes8 |
  bes4 c8 c[ bes] a |
  bes4.~ bes4 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  bes8 |
  bes4 bes8 bes4 bes8 |
  bes4 a8 bes4 bes8 |
  ees[ f] g g4 c,8 |
  f4.~ f4 |
  
  bes,8 |
  bes4 bes8 bes4 bes8 |
  bes4 a8 bes4 d8 |
  ees4 c8 f4 f8 |
  bes,4.~ bes4 |
  
  d8 |
  d4 d8 d4 c8 |
  bes4 d8 g4 bes,8 |
  c4 c8 c4 c8 |
  f4.~ f4 |
  
  f8 |
  bes,4 bes8 bes4 bes8 |
  bes4 a8 bes4 d8 |
  ees4 c8 f4 f8 |
  bes,4.~ bes4 \bar "|."
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
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWords
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

