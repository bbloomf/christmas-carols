\version "2.24.0"
\include "util.ly"
\header {
  tagline = \markup { "from" \italic {CyberHymnal.org}}
}
\paper {
  print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -10)
       (stretchability . 100))
  score-markup-spacing =
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
  first-page-number = #004
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCaps advent}
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
  \key aes \major
  \time 4/4
  \autoBeamOff
}

sopMusic = \relative c' {
  c'4 aes ees ees |
  ees aes aes g |
  aes c f,8[ g] aes4 |
  des c c( bes) |
  
  aes des des c |
  bes f' f ees |
  ees des c8[ bes] aes4 |
  aes g aes2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  ees4 c des des |
  c c8[ d] ees4 ees |
  ees c f ees8[ f] |
  g4 aes aes( g) |
  
  aes8[ g] f4 ees aes8[ ges] |
  f4 f8[ g!] aes4 ges |
  f g!8[ f] e4 c8[ d] |
  ees4 ees8[ des] c2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsVII
  \set stanza = #"1. "
  Come, Thou long ex -- pect -- ed Je -- sus,
  born to set __ Thy peo -- ple free;
  from our fears and sins re -- lease us, let us find our rest in Thee.
}
altoWordsII = \lyricmode {
  \dropLyricsVII
%\markup\italic
  \set stanza = #"2. "
  Is -- rael’s strength and con -- so -- la -- tion,
  hope of all __ the earth Thou art:
  dear de -- sire of ev -- ’ry na -- tion, joy of ev -- ’ry long -- ing heart.
}
altoWordsIII = \lyricmode {
  \dropLyricsVII
  \set stanza = #"3. "
  Born Thy peo -- ple to de -- liv -- er, born a child, and yet a king,
  born to reign in us for ev -- er, now Thy gra -- cious king -- dom bring.
}
altoWordsIV = \lyricmode {
  \dropLyricsVII
  \set stanza = #"4. "
  By Thine own e -- ter -- nal Spi -- rit rule in all __ our hearts a -- lone;
  by Thy grace, help us to mer -- it life e -- ter -- nal at Thy throne.
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
  aes4 aes bes bes |
  aes aes8[ bes] c4 bes |
  aes aes aes8[ bes] c4 |
  des ees ees2 |
  
  c4 des8[ c] bes4 c |
  des bes c aes |
  aes bes8[ aes] g4 aes8[ bes] |
  c4 bes aes2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  aes4 aes' aes g |
  aes f ees ees8[ des] |
  c4 aes des c |
  bes aes ees'2 |
  
  f4 bes8[ aes] g4 aes |
  des, des c c |
  des bes c f |
  ees ees aes,2 \bar "|."
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
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women"  \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women"  \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women"  \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women"  \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women"  \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women"  \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWords
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
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Come Thou Long Expected Jesus"}}
    poet = \markup\oldStyleNum"Charles Wesley (1707–1788)"
    composer = \markup\concat{\italic"Cross of Jesus" \oldStyleNum", Sir John Stainer (1840–1901)"}
  }
}
\markup\fill-line \center-align {\concat{"from "\italic"The Church Hymnary" \oldStyleNum", 1902, via " \italic"HymnsAndCarolsOfChristmas.com"}}
\markup \vspace #1






























global = {
  \key g \major
  \time 4/4
  \autoBeamOff
}

sopMusic = \relative c' {
  d4 d g g |
  a a b g |
  d' d e c |
  a d b2 |
  
  b4 b a b |
  g a g fis |
  g e d g |
  g fis g2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d4 d b b |
  e d d d |
  d g g g |
  g fis g2 |
  g4 g fis fis |
  b, e d d |
  d c b d |
  d d d2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsVII
  \set stanza = #"1. "
  Come, Thou long ex -- pect -- ed Je -- sus,
  born to set Thy peo -- ple free;
  from our fears and sins re -- lease us, let us find our rest in Thee.
}
altoWordsII = \lyricmode {
  \dropLyricsVII
%\markup\italic
  \set stanza = #"2. "
  Is -- rael’s strength and con -- so -- la -- tion,
  hope of all the earth Thou art:
  dear de -- sire of ev -- ’ry na -- tion, joy of ev -- ’ry long -- ing heart.
}
altoWordsIII = \lyricmode {
  \dropLyricsVII
  \set stanza = #"3. "
  Born Thy peo -- ple to de -- liv -- er, born a child, and yet a king,
  born to reign in us for ev -- er, now Thy gra -- cious king -- dom bring.
}
altoWordsIV = \lyricmode {
  \dropLyricsVII
  \set stanza = #"4. "
  By Thine own e -- ter -- nal Spi -- rit rule in all our hearts a -- lone;
  by Thy grace, help us to mer -- it life e -- ter -- nal at Thy throne.
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
  b4 a g g |
  g fis g b |
  g b c e |
  d d d2 |
  
  e4 b c b |
  b a a a |
  g g d' b |
  a8[ b] c4 b2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  g'4 fis e d |
  c d g g, |
  b g c a |
  d d g2 |
  
  e4 e e dis |
  e cis d c |
  b c g b8[ c] |
  d4 d g,2 \bar "|."
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
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsV
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
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Come Thou Long Expected Jesus"}}
    poet = \markup\oldStyleNum"Charles Wesley (1707–1788)"
    composer = \markup\concat{\italic"Stuttgart" \oldStyleNum", Christian F. Witt (c. 1660–1716)"}
    arranger = \markup\oldStyleNum"Adapted by Henry J. Gauntlett (1805–1876)"
    tagline = \markup { "from" \italic "CyberHymnal.org"}
  }
}

