\version "2.14.2"
\include "util.ly"
\header {
  tagline = ""
}
\paper {
  print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  %system-system-spacing =
  %  #'((basic-distance . 0)
  %     (minimum-distance . 0)
  %     (padding . -0.35)
  %     (stretchability . 100))
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
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #38
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }

global = {
  \key f \major
  \time 3/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  f2 a4 |
  c2 a4 |
  bes( c) d |
  c2 g4 |
  a( bes) c |
  
  bes( a) g |
  f2 d4 |
  e2 c4 |
  a'2 bes4 |
  c2 d4 |
  
  c2 g4 |
  a2 f4 |
  bes2 bes4 |
  a( g) f |
  f( d) e |
  f2. \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  f2 f4 |
  g2 f4 |
  f2 f4 |
  f2 e4 |
  f2 f4 |
  
  d2 e4 |
  d2 b4 |
  c2 c4 |
  c2 f4 |
  f2 f4 |
  
  f2 e4 |
  e2 d4 |
  d2 g4 |
  e2 d4 |
  d2 c4 |
  c2. \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = "1. "
  Quem pas -- tó -- res lau -- da -- vé -- re,
  Qui -- bus án -- ge -- li di -- xé -- re,
  “Ab -- sit vo -- bis jam ti -- mé -- re,
  Na -- tus est rex gló -- ri -- æ.”
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = "2. "
  Ad quem ma -- gi am -- bu -- lá -- bant,
  Au -- rum, thus, myr -- rham por -- tá -- bant,
  Im -- mo -- lá -- bant hæc sin -- cé -- re
  Le -- ó -- ni vic -- tó -- ri -- æ.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = "3. "
  Ex -- ul -- té -- mus cum Ma -- rí -- a
  In cæ -- lés -- ti hie -- rár -- chi -- a
  Na -- tum pro -- mat vo -- ce pi -- a
  Laus, ho -- nor et gló -- ri -- a.
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = "4. "
  Chris -- to re -- gi, De -- o na -- to,
  Per Ma -- rí -- am no -- bis da -- to,
  Mé -- ri -- to ré -- so -- net ve -- re
  Dul -- ci cum me -- ló -- di -- a.
}
altoWordsV = \lyricmode {
  \set stanza = "5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = "6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  a2 c4 |
  c2 c4 |
  bes2 bes4 |
  g( a) bes |
  a2 a4 |
  
  bes2 bes4 |
  a2 g4 |
  g2 e4 |
  f2 f4 |
  f2 bes4 |
  
  g( a) bes |
  a2 a4 |
  bes2 d4 |
  c2 a4 |
  bes2 bes4 |
  a2. \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  f2 f4 |
  e2 f4 |
  d2 bes4 |
  c2 c4 |
  f2 a4 |
  
  g2 c,4 |
  d2 g4 |
  c,2 c4 |
  f2 d4 |
  a2 bes4 |
  
  c2 c4 |
  cis2 d4 |
  g,2 g4 |
  a2 d4 |
  g,2 c4 |
  f2. \bar "|."
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
    \new Lyrics \with { alignAboveContext = "women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = "women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = "men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = "men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText #'font-size = #2
  }
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
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Quem Pastores"}}
%    poet = \markup\oldStyleNum"Anonymous, 14th Century"
%    composer = \markup\oldStyleNum"14th Century German"
    composer = \markup\oldStyleNum"Arranged by Ralph Vaughan Williams (1872–1958)"
    tagline = \markup { "from" \italic {ChristmasCarolMusic.org}}
  }
}






altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = "1. "
  While their flocks the shep -- herds tend -- ed
  Heav’n -- ly hosts to earth de -- scend -- ed
  Sing -- ing, with all voic -- es blend -- ed,
  “Fear not, Christ is born to -- day.”
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = "2. "
  East -- ern Seers rich gifts had wrought Him
  Gold, Frank -- in -- cense, myrrh they brought Him
  Guid -- ed by a Star, they sought Him
  Prince of Life and Vic -- to -- ry.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = "3. "
  On that Child with Mar -- y gaz -- ing,
  Join ye Chris -- tians all in rais -- ing
  Songs to Him, whom Heav’n is prais -- ing,
  God in -- car -- nate come to men.
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = "4. "
  From this day’s first dawn to e -- ven
  Praise to Christ our King be giv -- en
  By all Earth, and all in Heav -- en,
  In our sweet -- est, loft -- iest strain.
}
\score {
  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = "women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = "women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = "men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = "men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText #'font-size = #2
  }
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
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Quem Pastores"}}
%    poet = \markup\oldStyleNum"Anonymous, 14th Century"
%    composer = \markup\oldStyleNum"14th Century German"
%    composer = \markup\oldStyleNum"Arranged by Ralph Vaughan Williams (1872–1958)"
%    tagline = \markup { "from" \italic {ChristmasCarolMusic.org}}
  }
}
