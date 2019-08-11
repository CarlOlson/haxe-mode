Feature: Syntax Highlighting
  Scenario: Double quote string
    Given I am in a haxe buffer
    And I insert
    """
    "hello world";
    """
    When at line 1
    Then expect font-lock-string-face

  Scenario: Single quote string
    Given I am in a haxe buffer
    And I insert
    """
    'hello world';
    """
    When at line 1
    Then expect font-lock-string-face

  Scenario: Line comment
    Given I am in a haxe buffer
    And I insert
    """
    // comment
    """
    When at point 4
    Then expect font-lock-comment-face

  Scenario: Multiline comment
    Given I am in a haxe buffer
    And I insert
    """
    /* comment */
    """
    When at point 4
    Then expect font-lock-comment-face