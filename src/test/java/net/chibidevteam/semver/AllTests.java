package net.chibidevteam.semver;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import net.chibidevteam.semver.constraint.ComparisionTests;
import net.chibidevteam.semver.serialization.SerializationTests;

@RunWith(Suite.class)
@SuiteClasses({ SerializationTests.class, ComparisionTests.class })
public class AllTests {
}
