package com.fackbook.Configuration;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.Module;
import com.fasterxml.jackson.databind.module.SimpleModule;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.io.IOException;

@Configuration
public class JacksonConfig {

    @Bean
    public Module caseInsensitiveEnumModule() {
        SimpleModule module = new SimpleModule();
        module.setDeserializerModifier(new CaseInsensitiveEnumDeserializerModifier());
        return module;
    }

    public static class CaseInsensitiveEnumDeserializer<T extends Enum<T>> extends JsonDeserializer<T> {
        private final Class<T> enumType;

        public CaseInsensitiveEnumDeserializer(Class<T> enumType) {
            this.enumType = enumType;
        }

        @Override
        public T deserialize(JsonParser p, DeserializationContext ctxt) throws IOException, JsonProcessingException {
            String text = p.getText();
            for (T constant : enumType.getEnumConstants()) {
                if (constant.name().equalsIgnoreCase(text)) {
                    return constant;
                }
            }
            ctxt.reportInputMismatch(enumType, "Value '%s' is not valid for enum %s", text, enumType.getSimpleName());
            return null; // unreachable
        }
    }

    public static class CaseInsensitiveEnumDeserializerModifier extends com.fasterxml.jackson.databind.deser.BeanDeserializerModifier {
        @Override
        public JsonDeserializer<?> modifyEnumDeserializer(
                com.fasterxml.jackson.databind.DeserializationConfig config,
                com.fasterxml.jackson.databind.JavaType type,
                com.fasterxml.jackson.databind.BeanDescription beanDesc,
                JsonDeserializer<?> deserializer) {
            Class<?> rawClass = type.getRawClass();
            if (Enum.class.isAssignableFrom(rawClass)) {
                @SuppressWarnings("unchecked")
                Class<Enum<?>> enumClass = (Class<Enum<?>>) rawClass;
                return new CaseInsensitiveEnumDeserializer(enumClass);
            }
            return deserializer;
        }
    }
}
