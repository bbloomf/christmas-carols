\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Come! Tune Your Heart"}}
  poet = \markup\concat{\italic"Auf, schicke dich" ", by Christian Fürchtegott Gellert (1715–1769)"}
  meter = \markup\oldStyleNum"Translated by Frances E. Cox (1812–1897)"
  composer = \markup\oldStyleNum"Sir Frederick A. G. Ouseley (1825–1889)"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  system-system-spacing = #'((basic-distance . 10) (padding . 0))
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
  first-page-number = #126
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
  \key ees \major
  \time 2/2
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-0.5 . 0.5)
}

sopMusic = \relative c' {
  \partial 2 ees2 |
  g aes |
  bes g |
  aes c |
  \partial 2 bes \bar "||"
  \partial 2 bes |
  ees d4( c) |
  
  bes2 a4( g) |
  f2 c' |
  <<bes {s4 s^\ff}>> d2 |
  f1 |
  d2 bes |
  c1 |
  \partial 2 bes2 \bar "||" \break
  
  \partial 2 bes2 |
  g f |
  ees c' |
  aes g |
  \partial 2 f2 \bar "||"
  \partial 2 g2 |
  aes bes |
  
  c c d ees4( c) |
  bes2 bes |
  c1 |
  bes2 bes |
  f1 |
  \partial 2 ees2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  bes2 |
  ees ees |
  d ees |
  ees ees |
  ees |
  f |
  ees f |
  
  g c, |
  d ees |
  d bes' |
  bes( a) |
  bes bes |
  bes( a) |
  bes |
  
  ees, |
  ees d |
  ees f |
  f e |
  f
  ees? |
  ees ees |
  
  ees ees |
  aes aes |
  aes g |
  g( f4 ees) |
  f2 ees |
  ees( d) |
  ees2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  Come! tune your heart, To bear its part,
  And ce -- le -- brate Mes -- si -- ah’s feast with prais -- es,
  \set associatedVoice = "tenors"
  with prais -- es;
  
  Let love in -- spire The joy -- ful choir,
  While to the God of Love, glad Hymns
  \unset associatedVoice
  it rais -- es,
  \set associatedVoice = "tenors"
  it rais -- es.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  Ex -- alt His Name; With joy pro -- claim,
  God loved the world, and through His Son for -- gave us, 
  \set associatedVoice = "tenors"
  for -- gave us;
  Oh! what are we,
  That, Lord, we see
  Thy won -- drous love, in Christ who died 
  \unset associatedVoice
  to save __ us,
  \set associatedVoice = "tenors"
  to save __ us!
}
altoWordsIII = {
  \dropLyricsIX
  \set stanza = \markup{\dynamic"mf " "3. "}
  \lyricmode {
    Your ref -- uge place In His free grace,
    Trust in His Name, and day by day re -- pent you, 
    \set associatedVoice = "tenors"
    re -- pent you;
    Ye mock God’s word, Who call Him Lord,
    And fol -- low not the pat -- tern He 
    \unset associatedVoice
    hath lent __ you,
    \set associatedVoice = "tenors"
    hath lent __ you.
  }
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  O Christ, to prove For Thee, my love,
  In breth -- ren Thee my hands shall clothe and cher -- ish, 
  \set associatedVoice = "tenors"
  and cher -- ish;
  To each sad heart Sweet Hope im -- part,
  When worn with care, with sor -- row nigh 
  \unset associatedVoice
  to per -- ish,
  \set associatedVoice = "tenors"
  to per -- ish.
}
altoWordsV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"5. "
  Come! praise the Lord; In Heav’n are stored
  Rich gifts for those who here His Name e -- steem -- ed, 
  \set associatedVoice = "tenors"
  e -- steem -- ed;
  Al -- le -- lu -- ia; Al -- le -- lu -- ia;
  Re -- joice in Christ, and praise Him ye 
  \unset associatedVoice
  re -- deem -- ed,
  \set associatedVoice = "tenors"
  re -- deem -- ed.
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  g2 |
  g ees |
  f ees |
  aes aes |
  g |
  bes |
  c bes4( aes) |
  
  g2 bes |
  bes a |
  <<bes {s4 s_\ff}>> bes2 |
  c1 |
  d2 e |
  f1 |
  d2 |
  
  bes |
  c aes |
  g c |
  des bes |
  aes |
  bes |
  aes g |
  
  aes aes |
  aes c |
  f ees |
  ees( c) |
  d ees4( bes) |
  bes2.( aes4) |
  g2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  ees2 |
  d c |
  bes ees |
  c aes |
  ees' |
  d |
  c d |
  
  ees e |
  f f |
  bes g |
  f1 |
  bes2 g |
  f1 |
  bes2 |
  
  g, |
  aes bes |
  c aes |
  bes c |
  des! |
  des! |
  c bes |
  
  aes aes'4( g) |
  f2 ees |
  d ees |
  aes,1 |
  bes4( aes) g2 |
  bes1 |
  ees2 \bar "|."
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
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "altos" \altoWords
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

