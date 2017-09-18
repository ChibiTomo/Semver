package net.chibidevteam.semver.serialization;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

@RunWith(Suite.class)
@SuiteClasses({ SerializerTest.class, DeserializerTest.class, VersionHelperTest.class })
public class SerializationTests {

}
