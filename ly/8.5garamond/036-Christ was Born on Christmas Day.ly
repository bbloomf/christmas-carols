\version "2.24.0"
\include "util.ly"
\header {
  tagline = \markup \concat { "from " \italic "The Cowley Carol Book" \oldStyleNum", 1919"}
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
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #036
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
  \key f \major
  \time 6/8
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c'' {
  \repeat volta 2{
    c4 a8 f4 a8 |
    c4 d8 c4. |
    c4 a8 f4 a8 |
    c4 d8 c4. | \break
    
    bes8[ bes] bes8 bes4 c8 |
    bes4 a8 g4 a8 |
    c4 a8 f4 bes8 |
    a4 g8 g4 a8 |
    f4. f\fermata \break
  }
  \repeat volta 2{
    c'4 a8 f4 a8 |
    c4 d8 c4. |
    c4 a8 f4 a8 |
    c4 d8 c4. | \break
    
    bes4 bes8 bes4 c8 |
    bes4 a8 g4 a8 |
    c4 a8 f4 bes8 |
    
    
    %page2
    a4 g8 g4 a8 |
    f4. f\fermata \break
  }
  
  f4 g8 a4 bes8 |
  a4 g8 a4 g8 |
  
  f4 e8 f4. |
  c'4. a |
  c a |
  f4 g8 a4 bes8 |
  
  a4 g8 a4. |
  f4 g8 a4 bes8 |
  c4 bes8 a4 bes8 |
  g4 g8 f4.\fermata \bar"||"
  
  c'4 c8 d4 e8 |
  f4 e8 d4 c8 |
  f4 e8 d4 c8 |
  c4 b8 c4. |
  
  f,4 g8 a4 bes8 c4 bes8 a4 bes8 |
  g4 g8 f4\fermata \bar"||"
  \teeny c'8 | \normalsize
  c4 a8 f4 a8 |
  
  %page3
  c4 d8 c4\fermata a8 |
  c4 d8 c4\fermata \teeny f,8 | \normalsize
  f4 g8 a4 bes8 |
  c4 bes8 a4 bes8 |
  
  g4 g8 f4.\fermata \bar"||"
  c'4 c8 d4 e8 f4 e8 d4 c8 |
  f4 e8 d4 c8 |
  
  c4 b8 c4. |
  f,4 g8 a4 bes8 |
  c4 bes8 a4 bes8 |
  g4 g8 f4.\fermata \bar"|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  \repeat volta 2{
    f4 f8 f4 f8 |
    f4 f8 f4. |
    f4 f8 f4 f8 |
    f4 f8 e4. |
    
    f8[ f] f8 f4 f8 |
    f4 f8 e4 f8 |
    g4 e8 d4 f8 |
    f4 f8 e4 e8 |
    d4. c
  }
  \repeat volta 2{
    e4 e8 d4 d8 |
    e[ a] f e4. |
    e4 e8 f4 f8 |
    
    e4 g8 e4. |
    ees4 ees8 ees4 ees8 |
    d4 d8 d4 f8 |
    g4 e8 f4 f8 |
    
    %page2
    f8[ e] d e4 e8 |
    d4. c |
  }
  f4 f8 f4 f8 |
  f4 e8 e4 e8 |
  
  d4 c8 c4. |
  e d |
  c f |
  f4 e8 f4 f8 |
  
  f4 e8 f4. |
  f4 e8 f4 f8 |
  e4 d8 d4 d8 |
  c4 c8 a4. \bar"||"
  
  f'4 c8 f4 g8 |
  f4 g8 f4 f8 |
  a4 g8 f4 e8 |
  f4 f8 e4. |
  
  f4 e8 f4 f8 |
  f4 f8 f4 f8 |
  f4 e8 c4 \bar"||"
  \teeny c'8 | \normalsize
  c4 a8 f4 f8 |
  
  %page3
  f4 f8 f4 c8 |
  f4 f8 e4 \teeny f8 | \normalsize
  f4 e8 f4 f8 |
  f4 f8 f4 f8 |
  
  f4 e8 f4. \bar"||"
  f4 g8 f4 g8 |
  bes4 g8 f4 f8 |
  a4 g8 f4 e8 |
  
  f4 f8 e4. |
  f4 e8 f4 f8 |
  e4 f8 f4 f8 |
  f[ d] e f4. \bar"|."
}
altoWords = \lyricmode {
  \set stanza = " 1."
  Christ was born on Christ -- mas Day,
  Wreathe the hol -- ly, twine the bay;
  
  \override Lyrics.LyricText.font-shape = #'italic
  Chris -- tus na -- tus hó -- di -- e:
  \revert Lyrics.LyricText.font-shape
  The Babe, the Son, the Ho -- ly One of Ma -- ry.
  
  \set stanza = " 3."
  Let the bright red ber -- ries glow
  Ev -- ’ry -- where in good -- ly show;
  
  \override Lyrics.LyricText.font-shape = #'italic
  Chris -- tus na -- tus hó -- di -- e:
  \revert Lyrics.LyricText.font-shape
  The Babe, the Son, the Ho -- ly One of Ma -- ry.
  
  \set stanza = " 5."
  Night of sad -- ness: Morn of glad -- ness
  ev -- er -- more:
  Ev -- er, ev -- er:
  Af -- ter ma -- ny trou -- bles sore,
  Morn of glad -- ness ev -- er -- more and ev -- er -- more.
  
  \set stanza = "   6."
  Mid -- night scarce -- ly pass’d and o -- ver,
  Draw -- ing to this ho -- ly morn,
  Ve -- ry ear -- ly, ve -- ry ear -- ly Christ was born.
  
  \set stanza = "   7."
  Sing out with bliss, His Name is this:
  Em -- man -- u -- el:
  As was fore -- told in days of old
  By Ga -- bri -- el.
  
  \set stanza = "   8."
  Mid -- night scarce -- ly pass’d and o -- ver,
  Draw -- ing to this ho -- ly morn,
  Ve -- ry ear -- ly, ve -- ry ear -- ly Christ was born.
}
altoWordsII = \lyricmode {
  \set stanza = " 2."
  He is born to set us free,
  He is born our Lord to be,
  
  \override Lyrics.LyricText.font-shape = #'italic
  Ex Ma -- rí -- a Vír -- gi -- ne:
  \revert Lyrics.LyricText.font-shape
  The God, the Lord, by all a -- dor’d for ev -- er.
  
  \set stanza = " 4."
  Chris -- tian men, re -- joice and sing;
  ’Tis the birth -- day of a King,
  
  \override Lyrics.LyricText.font-shape = #'italic
  Ex Ma -- rí -- a Vír -- gi -- ne:
  \revert Lyrics.LyricText.font-shape
  The God, the Lord, by all a -- dor’d for ev -- er.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = " 1."
  % \override Lyrics.LyricText.font-shape = #'italic
  Ré -- so -- net in láu -- di -- bus
  cum ju -- cún -- dis pláu -- si -- bus
  Si -- on cum fi -- dé -- li -- bus,
  Ap -- pá -- ru -- it quem gé -- nu -- it Ma -- rí -- a.

  \set stanza = " 3."
  Pú -- e -- ri con -- cí -- ni -- te,
  na -- to re -- gi psál -- li -- te,
  vo -- ce pi -- a dí -- ci -- te
  Ap -- pá -- ru -- it quem gé -- nu -- it Ma -- rí -- a.

  \set stanza = " 5."
  Sunt im -- plé -- ta quæ præ -- dí -- xit Gá -- bri -- el.
  E -- ia, E -- ia,
  vir -- go De -- um gé -- nu -- it,
  quem di -- ví -- na vó -- lu -- it cle -- mén -- ti -- a.

  \set stanza = "   6."
  Hó -- di -- e ap -- pá -- ru -- it, ap -- pá -- ru -- it in Is -- ra -- ël,
  Ex Ma -- rí -- a vír -- gi -- ne est na -- tus Rex.

  \set stanza = "   7."
  "" Ma -- gnum no -- men Dó -- mi -- ni Em -- má -- nu -- el,
  "" quod an -- nun -- ti -- á -- tum est per Gá -- bri -- el.

  \set stanza = "   8."
  Hó -- di -- e ap -- pá -- ru -- it, ap -- pá -- ru -- it in Is -- ra -- ël,
  Ex Ma -- rí -- a vír -- gi -- ne est na -- tus Rex.
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = " 2."
  % \override Lyrics.LyricText.font-shape = #'italic
  Chris -- tus na -- tus hó -- di -- e
  ex Ma -- rí -- a vír -- gi -- ne
  \set ignoreMelismata = ##t
  si -- ne \unset ignoreMelismata vi -- rí -- li sé -- mi -- ne
  Ap -- pá -- ru -- it quem gé -- nu -- it Ma -- rí -- a.
  
  \set stanza = " 4."
  Si -- on lau -- da Dó -- mi -- num
  Sal -- va -- tó -- rem hó -- mi -- num,
  pur -- ga -- tó -- rem crí -- mi -- num
  Ap -- pá -- ru -- it quem gé -- nu -- it Ma -- rí -- a.
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  \repeat volta 2{
    a4 c8 a4 c8 |
    a4 a8 a4. |
    a4 c8 a4 c8 |
    a4 bes8 g4. |
    
    bes8[ bes] bes8 d4 c8 |
    d4 c8 c4 c8 |
    c4 c8 a4 d8 |
    c4 c8 c4 c8 |
    bes4. a
  }
  \repeat volta 2{
    g4 c8 a4 f8 |
    a4 a8 a4. |
    g4 c8 a4 d8 |
    
    c4 b8 g4. |
    g4 g8 g4 g8 |
    g4 a8 bes4 d8 |
    e4 c8 a4 d8 |
    
    %page2
    c4 b8 c4 c8 |
    bes[ f g] a4. |
  }
  a4 bes8 c4 d8 |
  c4 c8 c4 c8 |
  
  a[ bes] g a4. |
  g f |
  g c |
  c4 bes8 c4 d8 |
  
  c4 c8 c4. |
  c4 bes8 c4 f,8 |
  g4 g8 f4 f8 |
  f4 e8 f4. \bar"||"
  
  a4 g8 a4 c8 |
  c4 c8 a4 a8 |
  c4 c8 a4 a8 |
  f4 f8 g4. |
  
  c4 bes8 c4 d8 |
  c4 d8 c4 d8 |
  c4 c8 a4 \bar"||"
  \teeny c8 | \normalsize
  c4 a8 f4 c'8 |
  
  %page3
  a4 a8 a4 f8 |
  a4 bes8 g4 \teeny f8 | \normalsize
  a4 c8 c4 d8 |
  c4 d8 d4 d8 |
  
  c4 c8 a4. \bar"||"
  a4 g8 a4 c8 |
  d4 c8 a4 a8 |
  c4 c8 a4 a8 |
  
  f4 f8 g4. |
  c4 bes8 c4 bes8 |
  g4 bes8 c4 d8 |
  c4 c8 a4. \bar"|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  \repeat volta 2 {
    f4 f8 f4 f8 |
    f4 d8 f4. |
    f4 f8 f4 f8 |
    f4 bes,8 c4. |
    
    d8[ d] d8 bes4 a8 |
    bes4 f'8 c4 f8 |
    e4 c8 d4 bes8 |
    f'4 c8 c4 a8 |
    bes4. f'\fermata
  }
  \repeat volta 2{
    c4 c8 d4 d8 |
    a'4 d,8 a4. |
    c4 c8 d4 d8 |
    
    a'4 g8 c,4. |
    ees4 ees8 ees4 c8 |
    g'4 g8 g4 d8 |
    c4 c8 d4 bes8 |
    
    %page2
    f'4 g8 c,4 a8 |
    bes4. f\fermata |
  }
  f'4 f8 f4 bes,8 |
  f'4 c8 a4 c8 |
  
  d[ bes] c f4. |
  c d |
  e f |
  a4 g8 f4 bes,8 |
  
  f'4 c8 f4. |
  a4 g8 f4 d8 |
  c4 g8 d'4 bes8 |
  c4 c8 f4.\fermata \bar"||"
  
  f4 e8 d4 c8 |
  a4 c8 d4 f8 |
  f4 c8 d4 a8 |
  d4 d8 c4. |
  
  a'4 g8 f4 d8 |
  a4 bes8 f'4 bes,8 |
  c4 c8 f4\fermata \bar"||"
  \teeny c'8 | \normalsize
  c4 a8 f4 f8 |
  
  %page3
  f4 d8 f4\fermata f8 |
  f4 bes,8 c4\fermata \teeny f8 | \normalsize
  d4 c8 f4 bes,8 |
  a4 bes8 d4 bes8 |
  
  c4 c8 f4.\fermata \bar"||"
  f4 e8 d4 c8 |
  bes4 c8 d4 f8 |
  f4 c8 d4 a8 |
  
  d4 d8 c4. |
  a'4 g8 f[ e] d |
  c4 d8 f4 bes,8 |
  c4 c8 f4.\fermata \bar"|."
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
    \new Lyrics \with { alignAboveContext = "women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = "men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = "men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
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
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Christ Was Born on Christmas Day"}}
    subtitle = \markup{\override #'(font-name . "Garamond Premier Pro"){ \abs-fontsize #12.5 "(Resonet in laudibus)"}}
    poet = \markup\concat  {\oldStyleNum"14th Century Latin carol, as found in " \italic "Piæ Cantiones" \oldStyleNum", 1582"}
    meter = \markup\oldStyleNum"English words by John Mason Neale (1818–1866)"
    composer = \markup\concat{\oldStyleNum"14th Century German melody, " \italic"Resonet in laudibus"}
    arranger = \markup\oldStyleNum"Arranged chiefly by G. R. Woodward (1848–1934)"
    tagline = \markup \concat { "from " \italic "The Cowley Carol Book" \oldStyleNum", 1919"}
  }
}

