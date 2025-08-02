package com.mechanicalorchard.imogen.jclscan.model;

import lombok.Builder;
import lombok.Data;
import org.springframework.lang.Nullable;

@Data
@Builder
public class JclStep {
    private String name;
    @Nullable
    private Program pgm;
    @Nullable
    private JclProc proc;
}