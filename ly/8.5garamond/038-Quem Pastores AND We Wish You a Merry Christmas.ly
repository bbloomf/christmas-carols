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
  \set stanza = #"1. "
  Quem pas -- tó -- res lau -- da -- vé -- re,
  Qui -- bus án -- ge -- li di -- xé -- re,
  “Ab -- sit vo -- bis jam ti -- mé -- re,
  Na -- tus est rex gló -- ri -- æ.”
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  Ad quem ma -- gi am -- bu -- lá -- bant,
  Au -- rum, thus, myr -- rham por -- tá -- bant,
  Im -- mo -- lá -- bant hæc sin -- cé -- re
  Le -- ó -- ni vic -- tó -- ri -- æ.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  Ex -- ul -- té -- mus cum Ma -- rí -- a
  In cæ -- lés -- ti hie -- rár -- chi -- a
  Na -- tum pro -- mat vo -- ce pi -- a
  Laus, ho -- nor et gló -- ri -- a.
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  Chris -- to re -- gi, De -- o na -- to,
  Per Ma -- rí -- am no -- bis da -- to,
  Mé -- ri -- to ré -- so -- net ve -- re
  Dul -- ci cum me -- ló -- di -- a.
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
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWords
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







global = {
  \key aes \major
  \time 3/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-0.5 . 0.5)
}

sopMusic = \relative c' {
  \partial 4 ees4 |
  aes4 aes8\noBeam bes aes\noBeam g |
  f4 f f |
  bes bes8\noBeam c bes\noBeam aes |
  g4 ees ees |
  
  c' c8\noBeam des c\noBeam bes |
  \slurDashed aes4 f ees8\noBeam( ees) |
  f4 bes g |
  \partial 2 aes2^\markup\italic"Fine" \bar "||" \break
  \partial 4 ees4 |
  aes aes aes |
  
  g2 g4 |
  aes g f |
  ees2 bes'4 |
  c bes aes |
  ees' ees, ees8\noBeam ees |
  f4 bes g |
  aes2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  ees4 |
  c c8\noBeam c c\noBeam c |
  des4 des ees |
  d d8\noBeam d d\noBeam d |
  ees4 ees ees |
  
  e e8\noBeam e e\noBeam e |
  \slurDashed f4 c ees8\noBeam( ees) |
  des4 f ees |
  ees2 \bar "||"
  c4 |
  ees4 ees ees |
  
  ees2 ees4 |
  d4 d d |
  ees2 ees4 |
  ees des c |
  ees ees ees8\noBeam ees |
  des4 f ees |
  ees2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1, 4. "
  \set ignoreMelismata = ##t
  We wish you a Mer -- ry Christ -- mas,
  We wish you a Mer -- ry Christ -- mas,
  We wish you a Mer -- ry Christ -- mas,
  And a hap -- py New Year!
}
altoWordsII = {
  \dropLyricsV
  \set stanza = \markup{\dynamic"mf   " "2. "}
  \lyricmode {
  %\markup\italic
    \set ignoreMelismata = ##t
    Oh, bring us a fig -- gy pud -- ding,
    Oh, bring us a fig -- gy pud -- ding,
    Oh, bring us a fig -- gy pud -- ding,
    and a cup of good cheer.
  }
  \set stanza = \markup\dynamic"mp "
  \lyricmode{
    Good ti -- dings to you wher -- ev -- er you are;
    Good ti -- dings for Christ -- mas and a hap -- py New Year!
  }
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  We won’t go un -- til we get some,
  We won’t go un -- til we get some,
  We won’t go un -- til we get some,
  so __ _ bring it right here.
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
}
tenorMusic = \relative c {
  ees4 |
  ees4 ees8\noBeam ees aes\noBeam aes |
  aes4 aes a |
  bes f8\noBeam aes g\noBeam f |
  ees4 g g |
  
  g g8\noBeam g c\noBeam c |
  \slurDashed c4 aes aes8\noBeam( aes) |
  aes4 des des |
  c2_\markup\italic"Fine" \bar "||"
  aes4 |
  c c c |
  
  bes2 bes4 |
  bes bes aes |
  g2 g4 |
  aes aes aes |
  aes aes aes8\noBeam aes |
  aes4 des des |
  c2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  ees4 |
  aes, aes8\noBeam aes aes\noBeam aes |
  des4 des c |
  bes bes8\noBeam bes bes\noBeam bes |
  ees4 ees des |
  
  c c8\noBeam c c\noBeam c |
  \slurDashed f4 f c8\noBeam( c) |
  des4 bes ees |
  aes2 \bar "||"
  aes4 |
  aes, aes aes |
  
  bes2 bes4 |
  bes bes bes |
  ees2 ees4 |
  aes, aes aes |
  c c c8\noBeam c |
  des4 bes ees |
  aes2 \bar "|."
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
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWords
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
    \Lyrics
    \override LyricText #'font-size = #1.3
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
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"We Wish You a Merry Christmas"}}
    poet = \markup\oldStyleNum"Traditional"
    composer = \markup\oldStyleNum"English Folk Song"
  }
}
