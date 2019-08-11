Feature: Code Comments
  Scenario: Single toplevel line
    Given I am in a haxe buffer
    And I insert
    """
    package;
    """
    When I 'comment-line
    Then I should have
    """
    // package;
    """

  Scenario: Single nested line
    Given I am in a haxe buffer
    And I insert
    """
    class MyClass {
      var name: String;
    }
    """
    When I 'comment-line at line 2
    Then I should have
    """
    class MyClass {
      // var name: String;
    }
    """