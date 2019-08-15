Feature: Indentation
  Scenario: Toplevel
    Given I am in a haxe buffer
    And I insert
    """
    package;
    """
    When I indent at line 1
    Then expect indent level 0

  Scenario: Inside block
    Given I am in a haxe buffer
    And I insert
    """
    class Main {
    var name:String;
    }
    """
    When I indent at line 2
    Then expect indent level 1

  Scenario: Closing block
    Given I am in a haxe buffer
    And I insert
    """
    class Main {
        var name:String;
    }
    """
    When I indent at line 3
    Then expect indent level 0

  Scenario: Nested blocks
    Given I am in a haxe buffer
    And I insert
    """
    class Main {
      function main() {

    """
    When I indent at line 3
    Then expect indent level 2

  Scenario: Multiline comments
    # Leave multiline comments alone for now
    Given I am in a haxe buffer
    And I insert
    """
    class Main {
        /* bad indent below
        */
    """
    When I indent at line 3
    Then expect indent level 1

  Scenario: Reindent after indention
    Given I am in a haxe buffer
    And I insert
    """
    class Main {
    // comment
    """
    When at line 2
    And I indent at column 2
    Then expect column at indent + 2

  Scenario: Reindent before indention
    Given I am in a haxe buffer
    And I insert
    """
    class Main {
      // comment
    """
    When at line 2
    And I indent at column 1
    Then expect column at indent + 0